-- |
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
module JReduce.OverDeep where

-- lens
import Control.Lens

-- jvmhs
import Jvmhs

-- containers
import qualified Data.IntSet as IS
import qualified Data.Set as S

-- unordered-containers
import qualified Data.HashSet as HS

-- text
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as Builder

-- base
import Data.Maybe
import qualified Data.List as List
import Data.Foldable

-- reduce-util
import Control.Reduce.Reduction
import Control.Reduce.Problem

-- vector
import qualified Data.Vector as V

-- jvmhs
import Jvmhs.Data.Code
import Jvmhs.Data.Signature
import Jvmhs.Transform.Stub
import Jvmhs.TypeCheck

-- jvm-binary
import qualified Language.JVM.ByteCode as B
import qualified Language.JVM.Type as B
import qualified Language.JVM.Constant as B

-- jreduce
import JReduce.Target
import JReduce.Config

data Item
  = IContent Content
  | ICode ((Class, Method), Code)
  | ITarget Target
  | ISuperClass (Class, ClassType)
  | IImplements (Class, ClassType)
  | IField (Class, Field)
  | IMethod (Class, Method)
  | IInnerClass (Class, InnerClass)

makePrisms ''Item

data Fact
  = ClassExist ClassName
  | CodeIsUntuched AbsMethodId
  | HasSuperClass ClassName ClassName
  | HasInterface ClassName ClassName
  | FieldExist AbsFieldId
  | MethodExist AbsMethodId
  | IsInnerClass ClassName ClassName
  | Meta
  deriving (Eq, Ord)

displayFact :: Fact -> Builder.Builder
displayFact = \case
  ClassExist cn -> toBuilder cn
  CodeIsUntuched md ->
    toBuilder md <> "!code"
  HasSuperClass cn1 cn2 ->
    toBuilder cn1 <> "<S]" <> toBuilder cn2
  HasInterface cn1 cn2 ->
    toBuilder cn1 <> "<I]" <> toBuilder cn2
  FieldExist fd ->
    toBuilder fd
  MethodExist md ->
    toBuilder md
  IsInnerClass _ cn2 ->
    toBuilder cn2 <> "!isinner"
  Meta -> "meta"

data EdgeSelection = EdgeSelection
  { edgeSelectInterfacesToMethods :: Bool
  , edgeSelectMethodsToMethods    :: Bool
  , edgeSelectMethodsToCode       :: Bool
  } deriving (Show, Eq, Ord)

instance Semigroup EdgeSelection where
  a <> b = EdgeSelection
    (any edgeSelectInterfacesToMethods [a, b])
    (any edgeSelectMethodsToMethods [a, b])
    (any edgeSelectMethodsToCode [a, b])

instance Monoid EdgeSelection where
  mempty = EdgeSelection False False False

describeProblem ::
  MonadIOReader Config m
  => EdgeSelection
  -> FilePath
  -> Problem a Target
  -> m (Problem a [IS.IntSet])
describeProblem es wf p = do
  describeProblemTemplate itemR genKeyFun displayFact _ITarget wf p

  where
    genKeyFun = do
      let targets = targetClasses $ _problemInitial p
      let scope = S.fromList ( map (view className) targets)
      hry <- fetchHierachy targets
      pure $ keyFun es scope hry

classConstructors :: Fold Class AbsMethodId
classConstructors =
  classAbsMethodIds . filtered (elemOf methodName "<init>")

classInitializers :: Fold Class AbsMethodId
classInitializers =
  classAbsMethodIds . filtered (elemOf methodName "<clinit>")

keyFun :: EdgeSelection -> S.Set ClassName -> Hierarchy -> Item -> (Fact, [Fact])
keyFun es scope hry = \case
  IContent (ClassFile cls) ->
    ( ClassExist $ cls ^.className
    , concat
      [ (( classBootstrapMethods . traverse . classNames
         <> classTypeParameters . traverse . classNames
          <> classEnclosingMethod . _Just . (_1 <> _2 . _Just . classNames)
        ) . to (makeClassExist cls) )
        `toListOf` cls
      , if cls ^. classAccessFlags . contains CEnum
        then cls ^.. classAbsFieldIds . to FieldExist
        else []
      , [ MethodExist mname
        | m <- cls ^. classMethods
        , let mname = mkAbsMethodId cls m
        , m' <- declarations hry mname
        , not (scope ^. contains (m' ^.className))
        ]
        -- If the class is an innerclass it needs to reference that
      , [ IsInnerClass (cls ^.className) (cls ^.className) ]

      -- If a field is synthetic it can exist for multiple
      -- reasons:
      --   - It is used to preload values to embeded
      --     methods.
      , [ FieldExist (mkAbsFieldId cls f)
        | f <- cls ^. classFields
        , f ^. fieldAccessFlags . contains FSynthetic
        ]
      ]
    )

  IField (cls, field) ->
    ( FieldExist $ mkAbsFieldId cls field
    , flip toListOf field . fold $
      [ classNames . to ClassExist
      , fieldAccessFlags . folding
        (\a ->
          if FFinal `S.member` a
          then
            CodeIsUntuched <$>
            if FStatic `S.member` a
            then toListOf classInitializers cls
            else toListOf classConstructors cls
          else []
        )
      ]
      --ClassExist cn : map CodeIsUntuched (toListOf classConstructors cls)
    )


  IInnerClass (cls, ic) ->
    ( IsInnerClass (cls^.className) $ ic ^. innerClass
    , toListOf (classNames . to ClassExist) ic
    )

  -- You can remove an implements statement if you can remove the class
  IImplements (cls, ct) ->
    ( HasInterface (cls^.className) (ct^.classTypeName)
    , concat
      [ ct ^..classNames.to (makeClassExist cls)
      , [ MethodExist (mkAbsMethodId (cls^.className) m)
        | edgeSelectInterfacesToMethods es
        , m <- cls^..classMethods.folded.methodId
        , abstractDeclaration hry (mkAbsMethodId (ct^.classTypeName) m)
        ]
      ]
    )

  ISuperClass (cls, ct) ->
    ( HasSuperClass (cls^.className) (ct^.classTypeName)
    , concat
      [ toListOf (classConstructors.to CodeIsUntuched) cls
      , ct ^..classNames.to (makeClassExist cls)
      , [ MethodExist (mkAbsMethodId (cls^.className) m)
        | edgeSelectInterfacesToMethods es
        , m <- cls^..classMethods.folded.methodId
        , abstractDeclaration hry (mkAbsMethodId (ct^.classTypeName) m)
        ]
      ]
    )

  IMethod (c, m) ->
    ( MethodExist mname
    , concat
      [ map (makeClassExist c) $ toListOf methodClassNames m
      -- This rule is added to handle cases where the interface is generic.
      -- In this case an synthetic method with the correct types are created.
      , [ CodeIsUntuched mname
        | m^.methodAccessFlags.contains MSynthetic
          || edgeSelectMethodsToCode es
        ]


      , concat
        [ [ makeClassExist c a ]
          ++ ( a `requireSubtype` ("java/lang/Throwable" :: ClassName))
        | a <- m^. methodExceptions
        ]

      -- If a method is abstact find it's definitions.
      , [ MethodExist (mkAbsMethodId cn m)
        | edgeSelectMethodsToMethods es
        , m ^. methodAccessFlags . contains MAbstract
        , cn <- HS.toList $ definitions hry mname
        , cn /= c ^. className
        ]
      ]
    )
    where
      mname = mkAbsMethodId (c^.className) (m ^. methodId)

      methodClassNames =
        methodDescriptor . classNames
        <> methodSignature . _Just . classNames

  ICode ((cls, m), code) ->
    ( CodeIsUntuched (mkAbsMethodId cls m)
    , codeDependencies cls m code
    )

  ITarget _ -> (Meta, [])
  IContent _ -> (Meta, [])


  where
    makeClassExist :: Class -> ClassName -> Fact
    makeClassExist cls thcls
      | has (fullyQualifiedName.to (Text.findIndex (=='$'))._Just) thcls
      = IsInnerClass (cls^.className) thcls
      | otherwise = ClassExist thcls

    codeDependencies :: Class -> Method -> Code -> [Fact]
    codeDependencies cls m code =
      toListOf
        ( ( codeExceptionTable.folded.classNames
            <> codeStackMap._Just.classNames
            <> codeByteCode.folded.classNames
          )
          . to (makeClassExist cls)
        ) code ++ processingCode cls m code


    processingCode :: Class -> Method -> Code -> [Fact]
    processingCode cls m code =
      case typeCheck hry
           (mkAbsMethodId cls m)
           (m^.methodAccessFlags.contains MStatic) code of
        Left (i, x) -> error
          (show (mkAbsMethodId cls m)
            ++ " "
            ++ show (code^?codeByteCode.ix i)
            ++ " "
            ++ show x)
        Right vc ->
          concat (V.zipWith (\ts c -> processOpCode ts (B.opcode c)) vc (code ^. codeByteCode))

    processOpCode :: TypeCheckState -> B.ByteCodeOpr B.High -> [Fact]
    processOpCode tcs = \case
      B.ArrayStore _ ->
        (tcs^?!tcStack.ix 0 `requireSubtype` tcs^?!tcStack.ix 2._TRef._Single._JTArray)
      B.Get fa fid ->
        findField fa (tcs^?!tcStack.ix 0) fid
      B.Put fa fid ->
        ( tcs^?!tcStack.ix 0 `requireSubtype` fid ^. fieldType )
        ++ findField fa (tcs^?!tcStack.ix 1) fid
      B.Invoke a ->
        case a of
          B.InvkSpecial (B.AbsVariableMethodId _ m') ->
            findMethod False m'
          B.InvkVirtual m' ->
            findMethod False m'
          B.InvkStatic (B.AbsVariableMethodId _ m') ->
            findMethod True m'
          B.InvkInterface _ (B.AbsInterfaceMethodId m') ->
            findMethod False m'
          B.InvkDynamic (B.InvokeDynamic _ m') ->
            concat $ zipWith requireSubtype (tcs^.tcStack)
            (reverse . map asTypeInfo $ m'^.methodArgumentTypes)

      B.Throw ->
        ( tcs^?!tcStack.ix 0 `requireSubtype` ("java/lang/Throwable" :: ClassName))

      _ -> []

      where
        findField :: B.FieldAccess -> TypeInfo -> AbsFieldId -> [Fact]
        findField fa ti fid =
          case fieldLocation hry fid of
            Just fid' ->
              [ FieldExist fid' ] ++
              concat [ ti `requireSubtype` fid' ^. className | fa == B.FldField]
            Nothing ->
              concat [ ti `requireSubtype` fid ^. className  | fa == B.FldField]

        
        findMethod :: Bool -> InRefType MethodId -> [Fact]
        findMethod isStatic m' =
          concat $
          [ [ MethodExist m'' ]
            ++ concat
            [ tcs^?!tcStack.ix (m''^.methodArgumentTypes.to length)
              `requireSubtype` (m''^.className)
            | not isStatic ]
          | m'' <- maybeToList $ declaration hry (AbsMethodId $ m'^.asInClass)
          ]
          ++ (zipWith requireSubtype (tcs^.tcStack)
              (reverse $ map asTypeInfo (m'^.methodArgumentTypes))
             )

    infixl 5 `requireSubtype`
    requireSubtype ::
      (AsTypeInfo a, AsTypeInfo b)
      => a -> b -> [Fact]
    requireSubtype a' b' =
      case (asTypeInfo a', asTypeInfo b') of
        (TRef as, TRef bs) -> concat
          [ a `requireSubReftype` b
          | a <- toList as, b <- toList bs
          ]
        (a         , b      )
          | a == b -> []
          | otherwise -> error "Type error"

      where
        requireSubReftype = \case
          B.JTClass s -> \case
            B.JTClass "java/lang/Object" ->
              -- A class will always extend java/lang/Object
              []
            B.JTClass t ->
              [ case edge of
                  Extend -> HasSuperClass cn1 cn2
                  Implement -> HasInterface cn1 cn2
              | (cn1, cn2, edge) <-
                fromMaybe (error $ "Type error: " ++ show s ++ " !<: " ++ show t )
                $ subclassPath hry s t
              ]
            _ -> error "Type error"
          B.JTArray s -> \case
            B.JTArray t ->
              case (s, t) of
                (JTRef s', JTRef t') -> s' `requireSubReftype` t'
                _
                  | s == t -> []
                  | otherwise -> error "Type error"
            B.JTClass t
              | List.elem t
                [ "java/lang/Object"
                , "java/lang/Cloneable"
                , "java.io.Serializable"] -> []
              | otherwise -> error "Type error"

itemR :: PartialReduction Item Item
itemR f' = \case
  ITarget t ->
    fmap ITarget <$> targetR f' t
  IContent c ->
    fmap IContent <$> contentR f' c
  IMethod (c, m) ->
    fmap (IMethod . (c,)) <$> (part $ methodR c) f' m
  a -> pure (Just a)
  where
    contentR :: PartialReduction Content Item
    contentR f = \case
      ClassFile c -> fmap ClassFile <$> (part classR) f c
      Jar c       -> fmap Jar <$> (deepDirForestR . reduceAs _IContent) f c
      a           -> pure $ Just a

    targetR :: PartialReduction Target Item
    targetR = deepDirTreeR . reduceAs _IContent

    classR :: Reduction Class Item
    classR f c = do
      (super :: Maybe ClassType) <- case c ^. classSuper of
        Just a
          | a ^. classTypeName == "java/lang/Object" ->
            pure $ Just  a
          | otherwise ->
           (payload c . reduceAs _ISuperClass) f a <&> \case
             Just a' -> Just a'
             Nothing -> Just (ClassType "java/lang/Object" [])
        Nothing ->
          pure $ Nothing

      fields <-
        (listR . payload c . reduceAs _IField) f (c ^. classFields)

      methods <-
        (listR . payload c . reduceAs _IMethod) f (c ^. classMethods)

      innerClasses <-
        (listR . payload c . reduceAs _IInnerClass) f (c ^. classInnerClasses)

      interfaces <-
        (listR . payload c . reduceAs _IImplements) f (c ^. classInterfaces)

      pure $ c
        & classSuper .~ super
        & classFields .~ fields
        & classMethods .~ methods
        & classInnerClasses .~ innerClasses
        & classInterfaces .~ interfaces

    methodR :: Class -> Reduction Method Item
    methodR cls f m =
      case m ^. methodCode of
        Just c -> f (ICode ((cls, m), c)) <&> \case
          Just (ICode (_, c')) -> m & methodCode ?~ c'
          _ -> stub m
        _ -> pure m

payload ::
  Functor f =>
  p
  -> ((p, a) -> f (Maybe (p, a)))
  -> a -> f (Maybe a)
payload p fn a =
  fmap snd <$> fn (p, a)
