-- |
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
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
{-# OPTIONS -Wno-unused-imports #-}
module JReduce.OverDeep where

-- lens
import           Control.Lens

-- jvmhs
import           Jvmhs

-- containers
import qualified Data.IntSet                   as IS
import qualified Data.Set                      as S

-- text
import qualified Data.Text                     as Text
import qualified Data.Text.Lazy.Builder        as Builder

-- base
import           Data.Maybe
import qualified Data.List                     as List
import           Data.Foldable
import           GHC.Generics                   ( Generic )

-- deepseq
import           Control.DeepSeq

-- reduce-util
import           Control.Reduce.Reduction
import           Control.Reduce.Problem

-- vector
import qualified Data.Vector                   as V

-- containers
import qualified Data.IntMap.Strict            as IntMap

-- unorderd-containers
import qualified Data.HashMap.Strict           as HM

-- jvmhs
import           Jvmhs.Data.Code
import           Jvmhs.Data.Class
import           Jvmhs.Transform.Stub
import           Jvmhs.TypeCheck
import           Jvmhs.Inspection.ClassNames

-- jvm-binary
import qualified Language.JVM.ByteCode         as B
import qualified Language.JVM.Type             as B
import qualified Language.JVM.Constant         as B

-- jreduce
import           JReduce.Target
import           JReduce.Config

data Item
  = IContent Content
  | ICode ((Class, Method), Code)
  | ITarget Target
  | ISuperClass (Class, (Annotated ClassType))
  | IImplements (Class, (Annotated ClassType))
  | IField (Class, Field)
  | IFieldFinal (Class, Field)
  | IMethod (Class, Method)
  | IInnerClass (Class, InnerClass)
  -- | IMethodThrows ((Class, Method), (Annotated ThrowsType))
  | IBootstrapMethod (Class, (Int, BootstrapMethod))

makePrisms ''Item

data Fact
  = ClassExist ClassName
  | CodeIsUntuched AbsMethodId
  | HasSuperClass ClassName ClassName
  | HasInterface ClassName ClassName
  | FieldExist AbsFieldId
  | FieldIsFinal AbsFieldId
  | MethodExist AbsMethodId
  | IsInnerClass ClassName ClassName
  -- | MethodThrows AbsMethodId ClassName
  | HasBootstrapMethod ClassName Int
  | Meta
  deriving (Eq, Ord, Generic, NFData)

displayFact :: Fact -> Builder.Builder
displayFact = \case
  ClassExist     cn     -> toBuilder cn
  CodeIsUntuched md     -> toBuilder md <> "!code"
  HasSuperClass cn1 cn2 -> toBuilder cn1 <> "<S]" <> toBuilder cn2
  HasInterface  cn1 cn2 -> toBuilder cn1 <> "<I]" <> toBuilder cn2
  FieldExist  fd        -> toBuilder fd
  FieldIsFinal  fd      -> toBuilder fd <> "[final]"
  MethodExist md        -> toBuilder md
  IsInnerClass cn1 cn2  -> toBuilder cn1 <> "[innerOf]" <> toBuilder cn2
  -- MethodThrows m   cn   -> toBuilder m <> "[throws]" <> toBuilder cn
  HasBootstrapMethod cn b ->
    toBuilder cn <> "[bootstrap]" <> Builder.fromString (show b)
  Meta -> "meta"

data EdgeSelection = EdgeSelection
  { edgeSelectInterfacesToMethods :: Bool
  , edgeSelectMethodsToMethods    :: Bool
  , edgeSelectMethodsToCode       :: Bool
  , edgeSelectCodeToThrows        :: Bool
  } deriving (Show, Eq, Ord)

instance Semigroup EdgeSelection where
  a <> b = EdgeSelection (any edgeSelectInterfacesToMethods [a, b])
                         (any edgeSelectMethodsToMethods [a, b])
                         (any edgeSelectMethodsToCode [a, b])
                         (any edgeSelectCodeToThrows [a, b])

instance Monoid EdgeSelection where
  mempty = EdgeSelection False False False True

describeProblem
  :: MonadIOReader Config m
  => EdgeSelection
  -> FilePath
  -> Problem a Target
  -> m (Problem a [IS.IntSet])
describeProblem es wf p = describeProblemTemplate itemR
                                                  genKeyFun
                                                  displayFact
                                                  _ITarget
                                                  wf
                                                  p
 where
  genKeyFun = do
    let targets = targetClasses $ _problemInitial p
    let scope   = S.fromList (map (view className) targets)
    hry <- fetchHierachy targets
    pure $ \i -> do
      let (k, ks) = keyFun es scope hry i
      pure (k, map (k, ) ks)

classConstructors :: Fold Class AbsMethodId
classConstructors = classAbsMethodIds . filtered (elemOf methodIdName "<init>")

classInitializers :: Fold Class AbsMethodId
classInitializers =
  classAbsMethodIds . filtered (elemOf methodIdName "<clinit>")

keyFun
  :: EdgeSelection -> S.Set ClassName -> Hierarchy -> Item -> (Fact, [Fact])
keyFun es scope hry = \case
  IContent (ClassFile cls) ->
    ( ClassExist $ cls ^. className
    , concat
      [ ( (  classBootstrapMethods
          .  traverse
          .  classNames
          <> classTypeParameters
          .  traverse
          .  classNames
          <> classEnclosingMethod
          .  _Just
          .  (_1 <> _2 . _Just . classNames)
          )
        . to (makeClassExist cls)
        )
        `toListOf` cls
      , if cls ^. classAccessFlags . contains CEnum
        then cls ^.. classAbsFieldIds . to FieldExist
        else []

      -- If a method is an implementation of a method out of scope, then
      -- edgeSelectMethods will not work. To remain a valid program we
      -- add edges from the class itself to it.
      , [ MethodExist mname
        | edgeSelectMethodsToMethods es
        , m <- cls ^. classMethods
        , let mname = mkAbsMethodId cls m
        , (m', True) <- declarations mname hry
        , not (scope ^. contains (m' ^. className))
        ]
      -- If the class is an innerclass it needs to reference that
      , [IsInnerClass (cls ^. className) (cls ^. className)]

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
  
  IFieldFinal (cls, field) ->
    ( FieldIsFinal $ mkAbsFieldId cls field
    , []
    )

  IField (cls, field) ->
    ( FieldExist $ mkAbsFieldId cls field
    , flip toListOf field
      . fold
      $ [ classNamesOfField . to ClassExist
        , fieldAccessFlags . folding
          (\a -> if FFinal `S.member` a
            then CodeIsUntuched <$> if FStatic `S.member` a
              then toListOf classInitializers cls
              else toListOf classConstructors cls
            else []
          )
        ]
      --ClassExist cn : map CodeIsUntuched (toListOf classConstructors cls)
    )


  IInnerClass (cls, ic) ->
    ( IsInnerClass (cls ^. className) $ ic ^. innerClass
    , toListOf (classNamesOfInnerClass . to ClassExist) ic
    )

  IImplements (cls, ct) ->
    ( HasInterface (cls ^. className) (ct ^. simpleType)
    , concat
      [ ct ^.. classNames . to (makeClassExist cls)
      , [ MethodExist (mkAbsMethodId (cls ^. className) m)
        | edgeSelectInterfacesToMethods es
        , (m, (_, True)) <- HM.toList $ declaredMethods (ct ^. simpleType) hry
        ]
      ]
    )

  ISuperClass (cls, ct) ->
    ( HasSuperClass (cls ^. className) (ct ^. simpleType)
    , concat
      [ toListOf (classConstructors . to CodeIsUntuched) cls
      , ct ^.. classNames . to (makeClassExist cls)
      , [ MethodExist (mkAbsMethodId (cls ^. className) m)
        | edgeSelectInterfacesToMethods es
        , (m, (_, True)) <- HM.toList $ declaredMethods (ct ^. simpleType) hry
        ]
      ]
    )

  -- IMethodThrows ((c, m), t) ->
  --   ( MethodThrows (mkAbsMethodId c m) (t ^. simpleType)
  --   , concat
  --     [ [makeClassExist c a]
  --         ++ (a `requireSubtype` ("java/lang/Throwable" :: ClassName))
  --     | a <- m ^.. methodExceptions . folded . simpleType
  --     ]
  --   )

  IMethod (c, m) ->
    ( MethodExist mname
    , concat
      [ map (makeClassExist c) $ toListOf methodClassNames m
      -- This rule is added to handle cases where the interface is generic.
      -- In this case an synthetic method with the correct types are created.
      , [ CodeIsUntuched mname
        | m
          ^. methodAccessFlags
          .  contains MSynthetic
          || edgeSelectMethodsToCode es
        ]

      -- If a method is abstact find it's definitions.
      , [ MethodExist mid
        | edgeSelectMethodsToMethods es
        , m ^. methodAccessFlags . contains MAbstract
        , mid <- definitions mname hry
        , mid ^. className /= c ^. className
        ]
      ]
    )
   where
    mname = mkAbsMethodId (c ^. className) (m ^. methodId)

    methodClassNames =
      methodDescriptor
        .  classNames
        <> methodReturnType
        .  classNames
        <> methodParameters
        .  folded
        .  classNames

  ICode ((cls, m), code) ->
    ( CodeIsUntuched (mkAbsMethodId cls m)
    , codeDependencies cls m code
      -- <> [ MethodThrows (mkAbsMethodId cls m) (t ^. simpleType)
      --    | t <- m ^. methodExceptions
      --    ]
    )

  IBootstrapMethod (cls, (i, b)) ->
    ( HasBootstrapMethod (cls ^. className) i
    , b ^.. classNames . to (makeClassExist cls)
    )

  ITarget  _ -> (Meta, [])
  IContent _ -> (Meta, [])

 where
  makeClassExist :: Class -> ClassName -> Fact
  makeClassExist cls thcls
    | has (fullyQualifiedName . to (Text.findIndex (== '$')) . _Just) thcls
    = IsInnerClass (cls ^. className) thcls
    | otherwise
    = ClassExist thcls

  codeDependencies :: Class -> Method -> Code -> [Fact]
  codeDependencies cls m code =
    toListOf
        ( (  codeExceptionTable
          .  folded
          .  classNames
          <> codeStackMap
          .  _Just
          .  classNames
          <> codeByteCode
          .  folded
          .  classNames
          )
        . to (makeClassExist cls)
        )
        code
      ++ processingCode cls m code


  processingCode :: Class -> Method -> Code -> [Fact]
  processingCode cls m code =
    case
        typeCheck hry
                  (mkAbsMethodId cls m)
                  (m ^. methodAccessFlags . contains MStatic)
                  code
      of
        (Just (i, x), _) -> error
          (  show (mkAbsMethodId cls m)
          ++ " "
          ++ show (code ^? codeByteCode . ix i)
          ++ " "
          ++ show x
          )
        (Nothing, vc) -> concat
          (V.zipWith
            (\ts c -> processOpCode (m ^. methodIdReturnType) ts (B.opcode c))
            vc
            (code ^. codeByteCode)
          )

  processOpCode
    :: Maybe JType -> TypeCheckState -> B.ByteCodeOpr B.High -> [Fact]
  processOpCode rt tcs = \case
    B.ArrayStore _ ->
      (                tcs
      ^?!              tcStack
      .                ix 0
      `requireSubtype` tcs
      ^?!              tcStack
      .                ix 2
      .                _TRef
      .                _Single
      .                _JTArray
      )
    B.Get fa fid -> findField fa (tcs ^?! tcStack . ix 0) fid
    B.Put fa fid ->
      (tcs ^?! tcStack . ix 0 `requireSubtype` fid ^. fieldIdType)
        ++ findField fa (tcs ^?! tcStack . ix 1) fid
    B.Invoke a -> case a of
      B.InvkSpecial (B.AbsVariableMethodId _ m')    -> findMethod False m'
      B.InvkVirtual m'                              -> findMethod False m'
      B.InvkStatic  (B.AbsVariableMethodId _ m')    -> findMethod True m'
      B.InvkInterface _ (B.AbsInterfaceMethodId m') -> findMethod False m'
      B.InvkDynamic (B.InvokeDynamic _ m')          -> concat $ zipWith
        requireSubtype
        (tcs ^. tcStack)
        (reverse . map asTypeInfo $ m' ^. methodIdArgumentTypes)

    B.Throw ->
      (                tcs
      ^?!              tcStack
      .                ix 0
      `requireSubtype` ("java/lang/Throwable" :: ClassName)
      )

    B.CheckCast fa ->
      -- We just want to pick up on the intersection, if no subtype exist
      -- it's not a problem.
      (tcs ^?! tcStack . ix 0 `requireSubtype` fa)
        ++ (fa `requireSubtype` tcs ^?! tcStack . ix 0)

    B.Return (Just B.LRef) -> case rt of
      Just t  -> (tcs ^?! tcStack . ix 0 `requireSubtype` t)
      Nothing -> []
    _ -> []

   where
    findField :: B.FieldAccess -> TypeInfo -> AbsFieldId -> [Fact]
    findField fa ti fid = case fmap fst . uncons $ fieldLocations fid hry of
      Just fid' -> [FieldExist fid']
        ++ concat [ ti `requireSubtype` fid' ^. className | fa == B.FldField ]
      Nothing ->
        concat [ ti `requireSubtype` fid ^. className | fa == B.FldField ]

    findMethod :: Bool -> InRefType MethodId -> [Fact]
    findMethod isStatic m' =
      concat
        $  [ [MethodExist m'']
             ++ (m' ^. asInClass . className `requireSubtype` m'' ^. className)
             ++ concat
                  [ tcs
                    ^?! tcStack
                    . ix (m'' ^. methodIdArgumentTypes . to length)
                    `requireSubtype` (m'' ^. className)
                  | not isStatic
                  ]
           | (m'', _) <- take 1
             $ declarations (AbsMethodId $ m' ^. asInClass) hry
           ]
        ++ (zipWith requireSubtype
                    (tcs ^. tcStack)
                    (reverse $ map asTypeInfo (m' ^. methodIdArgumentTypes))
           )
  
  
  infixl 5 `requireSubtype`
  requireSubtype :: (AsTypeInfo a, AsTypeInfo b) => a -> b -> [Fact]
  requireSubtype a' b' = case (asTypeInfo a', asTypeInfo b') of
    (TRef as, TRef bs) ->
      concat [ a `requireSubReftype` b | a <- toList as, b <- toList bs ]
    (a, b) | a == b    -> []
           | otherwise -> error "Type error"

   where
    requireSubReftype = \case
      B.JTClass s -> \case
        B.JTClass "java/lang/Object" ->
          -- A class will always extend java/lang/Object
          []
        B.JTClass t ->
          [ case edge of
              Extend    -> HasSuperClass cn1 cn2
              Implement -> HasInterface cn1 cn2
          | (cn1, cn2, edge) <- case subclassPaths s t hry of
            a : _ -> take 1 $ subclassEdges a
            []    -> []
          ]
            -- fromMaybe (error $ "Type error: " ++ show s ++ " !<: " ++ show t )
        _ -> [] -- error "Type error"
      B.JTArray s -> \case
        B.JTArray t -> case (s, t) of
          (JTRef s', JTRef t') -> s' `requireSubReftype` t'
          _ | s == t    -> []
            | otherwise -> [] -- error "Type error"
        B.JTClass t
          | List.elem
            t
            ["java/lang/Object", "java/lang/Cloneable", "java.io.Serializable"]
          -> []
          | otherwise
          -> [] -- error "Type error"

itemR :: PartialReduction Item Item
itemR f' = \case
  ITarget  t      -> fmap ITarget <$> targetR f' t
  IContent c      -> fmap IContent <$> contentR f' c
  IMethod  (c, m) -> fmap (IMethod . (c, )) <$> (part $ methodR c) f' m
  IField   (c, m) -> fmap (IField . (c, ))  <$> (part $ fieldR c) f' m
  a               -> pure (Just a)
 where
  contentR :: PartialReduction Content Item
  contentR f = \case
    ClassFile c -> fmap ClassFile <$> (part classR) f c
    Jar       c -> fmap Jar <$> (deepDirForestR . reduceAs _IContent) f c
    a           -> pure $ Just a

  targetR :: PartialReduction Target Item
  targetR = deepDirTreeR . reduceAs _IContent

  classR :: Reduction Class Item
  classR f c = do
    _super <- case c ^. classSuper of
      Just a
        | a ^. annotatedContent . to classNameFromType == "java/lang/Object" -> pure
        $ Just a
        | otherwise -> (payload c . reduceAs _ISuperClass) f a <&> \case
          Just a' -> Just a'
          Nothing ->
            Just (withNoAnnotation (classTypeFromName "java/lang/Object"))
      Nothing -> pure $ Nothing

    fields <- (listR . payload c . reduceAs _IField) f (c ^. classFields)

    methods <- (listR . payload c . reduceAs _IMethod) f (c ^. classMethods)

    innerClasses <- (listR . payload c . reduceAs _IInnerClass)
      f
      (c ^. classInnerClasses)

    _interfaces <- (listR . payload c . reduceAs _IImplements)
      f
      (c ^. classInterfaces)

    bootstrapMethods <- 
      (iso IntMap.toAscList IntMap.fromAscList 
      . listR . payload c . reduceAs _IBootstrapMethod) 
      f (c ^.classBootstrapMethods)
 
    pure
      $  c &  classSuper .~ _super
      &  classFields .~ fields 
      &  classMethods .~ methods 
      &  classInnerClasses .~ innerClasses
      &  classInterfaces .~ _interfaces 
      &  classBootstrapMethods .~ bootstrapMethods
  
  fieldR :: Class -> Reduction Field Item
  fieldR cls fn f = do
    if f ^. fieldAccessFlags . contains FFinal
    then fn (IFieldFinal (cls, f)) <&> \case
      Just (IFieldFinal _) -> f
      _ -> f & fieldAccessFlags . at FFinal .~ Nothing
    else pure f

  methodR :: Class -> Reduction Method Item
  methodR cls f m = do
    t <- case m ^. methodCode of
      Just c -> f (ICode ((cls, m), c)) <&> \case
        Just (ICode (_, c')) -> Just c'
        _                    -> Nothing
      _ -> pure Nothing

    -- _methodThrows <- (listR . payload (cls, m) . reduceAs _IMethodThrows)
    --   f
    --   (m ^. methodExceptions)

    pure
      $  (case t of
           Just c  -> m & methodCode .~ Just c
           Nothing -> stub m
         )
      -- &  methodExceptions
      -- .~ _methodThrows


payload :: Functor f => p -> ((p, a) -> f (Maybe (p, a))) -> a -> f (Maybe a)
payload p fn a = fmap snd <$> fn (p, a)

methodIsAbstract :: Method -> Bool
methodIsAbstract = view (methodAccessFlags . contains MAbstract)
