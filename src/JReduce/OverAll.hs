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
module JReduce.OverAll where

-- lens
import Control.Lens

-- jvmhs
import Jvmhs

-- containers
import qualified Data.IntSet as IS
import qualified Data.Set as S

-- unordered-containers
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

-- text
import qualified Data.Text as Text

-- base
import Data.Maybe
import qualified Data.List as List
import Data.Monoid
import Data.Foldable
import Control.Monad
import Control.Monad.IO.Class

-- filepath
import System.FilePath

-- cassava
import qualified Data.Csv as C

-- bytestring
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Builder as BS

-- reduce-util
import Control.Reduce.Reduction
import Control.Reduce.Problem
import Control.Reduce.Graph
import qualified Control.Reduce.Util.Logger as L

-- vector
import qualified Data.Vector as V

-- jvmhs
import Jvmhs.Data.Named
import Jvmhs.Data.Code
import Jvmhs.Data.Signature
import Jvmhs.Transform.Stub
import Jvmhs.TypeCheck
-- import Jvmhs.Data.Type

-- jvm-binary
import qualified Language.JVM.ByteCode as B
import qualified Language.JVM.Type as B
import qualified Language.JVM.Constant as B
-- import qualified Language.JVM.Type as B
-- import qualified Language.JVM.Attribute.StackMapTable as B

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

instance C.ToField ([Int], Item) where
  toField (i, x) =
    BL.toStrict . BS.toLazyByteString
    $ "|" <> BS.stringUtf8 (List.intercalate "|". map show . reverse $ i)
    <> " " <> case x of
      IContent (Jar _) -> "jar"
      IContent (ClassFile c) ->
        BS.stringUtf8 (Text.unpack $ c^.className.fullyQualifiedName)
      IContent (MetaData _) -> "metadata"
      ICode ((cls, m), _) ->
        BS.stringUtf8 (Text.unpack . absMethodNameToText . mkAbsMethodName (cls ^. className) $ m ^.methodName )
        <> "!code"
      ITarget _ -> "base"
      IInnerClass (_, ic) ->
        BS.stringUtf8 (Text.unpack $ ic^.innerClass.fullyQualifiedName)
        <> "!isinner"
      ISuperClass (cls, ic) ->
        BS.stringUtf8 (Text.unpack $ cls^.className.fullyQualifiedName)
        <> "<S]" <>
        BS.stringUtf8 (Text.unpack $ ic^.classTypeName.fullyQualifiedName)
      IImplements (cls, ic) ->
        BS.stringUtf8 (Text.unpack $ cls^.className.fullyQualifiedName)
        <> "<I]" <>
        BS.stringUtf8 (Text.unpack $ ic^.classTypeName.fullyQualifiedName)
      IField (c, field) ->
        BS.stringUtf8
          . Text.unpack
          . absFieldNameToText
          $ mkAbsFieldName (c^.className) (field^.name)

      IMethod (c, method) ->
        BS.stringUtf8
          . Text.unpack
          . absMethodNameToText
          $ mkAbsMethodName (c^.className) (method^.name)

makePrisms ''Item

data Fact
  = ClassExist ClassName
  | CodeIsUntuched AbsMethodName
  | HasSuperClass ClassName ClassName
  | HasInterface ClassName ClassName
  | FieldExist AbsFieldName
  | MethodExist AbsMethodName
  | IsInnerClass ClassName ClassName
  deriving (Eq, Ord)

instance C.ToField Fact where
  toField = \case
    ClassExist _ -> "class"
    CodeIsUntuched _ -> "unstubbed"
    HasSuperClass _ _ -> "super"
    HasInterface _ _ -> "interface"
    FieldExist _ -> "field"
    IsInnerClass _ _ -> "isinnerclass"
    MethodExist _ -> "method"

targetClasses :: Target -> [Class]
targetClasses = toListOf (folded.go)
  where
    go :: Getting (Endo [Class]) Content Class
    go = _ClassFile <> _Jar.folded.go


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
  => FilePath
  -> EdgeSelection
  -> Problem a Target
  -> m (Problem a [IS.IntSet])
describeProblem wf es p = do
  let targets = targetClasses $ _problemInitial p
  let scope = S.fromList . map (view className) $ targets

  hry <- L.phase "Calculating the hierachy" $ do
    r <- preloadClasses

    hry <- fmap (snd . fst) . flip runClassPoolT
      (HM.fromList [ (c^.className, c) | c <- targets])
      $ do
      L.phase "Loading classes in class path" .  void 
        $ loadClassesFromReader (ReaderOptions False r)
      getHierarchy

    L.debug $ "Hierachy calculated, processed #"
      <> L.display (HM.size $ hry ^. hryStubs)
      <> " classes."

    return hry

  let
    p2 = liftProblem (review _ITarget) (fromJust . preview _ITarget) p

  L.phase "Outputing graph: " $ do
    liftIO
      . BL.writeFile (wf </> "graph.csv")
      . writeCSV
      . fst . reductionGraph (keyFun es scope hry) itemR
      $ _problemInitial p2

  return (toGraphReductionDeep (keyFun es scope hry) itemR p2)

classConstructors :: Fold Class AbsMethodName
classConstructors =
  classAbsMethodNames . filtered (elemOf methodId "<init>")

classInitializers :: Fold Class AbsMethodName
classInitializers =
  classAbsMethodNames . filtered (elemOf methodId "<clinit>")

keyFun :: EdgeSelection -> S.Set ClassName -> Hierarchy -> Item -> (Maybe Fact, [Fact])
keyFun es scope hry = \case
  IContent (ClassFile cls) ->
    ( Just (ClassExist $ cls ^.className)
    , concat
      [ (( classBootstrapMethods . traverse . classNames
         <> classTypeParameters . traverse . classNames
          <> classEnclosingMethod . _Just . (_1 <> _2 . _Just . classNames)
        ) . to (makeClassExist cls) )
        `toListOf` cls
      , if cls ^. classAccessFlags . contains CEnum
        then cls ^.. classAbsFieldNames . to FieldExist
        else []
      , [ MethodExist mname
        | m <- cls ^. classMethods
        , let mname = (mkAbsMethodName (cls^.className) (m^.name))
        , m' <- declarations hry mname
        , not (scope ^. contains (m' ^.inClassName))
        ]
        -- If the class is an innerclass it needs to reference that
      , [ IsInnerClass (cls ^.className) (cls ^.className) ]

      -- If a field is synthetic it can exist for multiple
      -- reasons:
      --   - It is used to preload values to embeded
      --     methods.
      , [ FieldExist (mkAbsFieldName (cls^.className) (f^.name))
        | f <- cls ^. classFields
        , f ^. fieldAccessFlags . contains FSynthetic
        ]
      ]
    )

  IField (cls, field) ->
    ( Just (FieldExist $ mkAbsFieldName (cls^.className) (field^.name) )
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
    ( Just (IsInnerClass (cls^.className) $ ic ^. innerClass)
    , toListOf (classNames . to ClassExist) ic
    )

  -- You can remove an implements statement if you can remove the class
  IImplements (cls, ct) ->
    ( Just (HasInterface (cls^.className) (ct^.classTypeName))
    , concat
      [ ct ^..classNames.to (makeClassExist cls)
      , [ MethodExist (mkAbsMethodName (cls^.className) m)
        | edgeSelectInterfacesToMethods es
        , m <- cls^..classMethods.folded.name
        , abstractDeclaration hry (mkAbsMethodName (ct^.classTypeName) m)
        ]
      ]
    )

  ISuperClass (cls, ct) ->
    ( Just (HasSuperClass (cls^.className) (ct^.classTypeName))
    , concat
      [ toListOf (classConstructors.to CodeIsUntuched) cls
      , ct ^..classNames.to (makeClassExist cls)
      , [ MethodExist (mkAbsMethodName (cls^.className) m)
        | edgeSelectInterfacesToMethods es
        , m <- cls^..classMethods.folded.name
        , abstractDeclaration hry (mkAbsMethodName (ct^.classTypeName) m)
        ]
      ]
    )

  IMethod (c, m) ->
    ( Just . MethodExist $ mname
    , concat
      [ map (makeClassExist c) $ toListOf methodClassNames m
      -- This rule is added to handle cases where the interface is generic.
      -- In this case an synthetic method with the correct types are created.
      , [ CodeIsUntuched mname
        | m^.methodAccessFlags.contains MSynthetic
          || edgeSelectMethodsToCode es
        ]


      -- If a method is abstact find it's definitions.
      , [ MethodExist (mkAbsMethodName cn (m ^. name))
        | edgeSelectMethodsToMethods es
        , m ^. methodAccessFlags . contains MAbstract
        , cn <- HS.toList $ definitions hry mname
        , cn /= c ^. className
        ]
      ]
    )
    where
      mname = mkAbsMethodName (c^.className) (m ^. name)

      methodClassNames =
        methodDescriptor . classNames
        <> methodExceptions . traverse
        <> methodSignature . _Just . classNames

  ICode ((cls, m), code) ->
    ( Just (CodeIsUntuched (mkAbsMethodName (cls^.className) $ m^.methodName))
    , codeDependencies cls m code
    )

  ITarget _ -> (Nothing, [])
  IContent _ -> (Nothing, [])


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
           (mkAbsMethodName (cls^.className) (m^.methodName))
           (m^.methodAccessFlags.contains MStatic) code of
        Left (i, x) -> error
          (show (mkAbsMethodName (cls^.className) (m^.methodName))
            ++ " "
            ++ show (code^?codeByteCode.ix i)
            ++ " "
            ++ show x)
        Right vc ->
          concat (V.zipWith (\ts c -> processOpCode ts (B.opcode c)) vc (code ^. codeByteCode))

    processOpCode :: TypeCheckState -> B.ByteCodeOpr B.High -> [Fact]
    processOpCode tcs = \case
      B.ArrayStore _ ->
        (tcs^?!tcStack.ix 0 `requireSubtype` tcs^?!tcStack.ix 2._VTObject._JTArray)
      B.Get fa fid ->
        [ FieldExist fid ]
        ++ concat [ tcs^?!tcStack.ix 0 `requireSubtype` fid ^.inClassName | fa == B.FldField ]
      B.Put fa fid ->
        [ FieldExist fid ]
        ++ ( tcs^?!tcStack.ix 0 `requireSubtype` fid ^. fieldType )
        ++ concat [ tcs^?!tcStack.ix 1 `requireSubtype` fid ^.inClassName | fa == B.FldField ]
      B.Invoke a ->
        case a of
          B.InvkSpecial (B.AbsVariableMethodId _ m') ->
            findMethod False m'
          B.InvkVirtual m' ->
            findMethod False m'
          B.InvkStatic  (B.AbsVariableMethodId _ m') ->
            findMethod True m'
          B.InvkInterface _ (B.AbsInterfaceMethodId m') ->
            findMethod False m'
          B.InvkDynamic (B.InvokeDynamic _ m') ->
            concat $ zipWith requireSubtype (tcs^.tcStack)
            (reverse . map asTypeInfo $ (review _Binary m' :: MethodName)^.methodArgumentTypes)
        where
          findMethod :: Bool -> AbsMethodName -> [Fact]
          findMethod isStatic m' =
            [ MethodExist m'' | m'' <- maybeToList $ declaration hry m']
            ++ (concat $ zipWith requireSubtype
              (tcs^.tcStack)
              (reverse $ [ asTypeInfo (m'^.inClassName) | not isStatic]
               ++ map asTypeInfo (m'^.methodArgumentTypes))
               )

      B.Throw ->
        ( tcs^?!tcStack.ix 0 `requireSubtype` ("java/lang/Throwable" :: ClassName))
      B.InstanceOf trg ->
        ( trg `requireSubtype` tcs^?!tcStack.ix 0)
      B.CheckCast trg ->
        ( trg `requireSubtype` tcs^?!tcStack.ix 0)

      _ -> []

    infixl 5 `requireSubtype`
    requireSubtype :: (AsTypeInfo a, AsTypeInfo b) => a -> b -> [Fact]
    requireSubtype a' b' =
      case (asTypeInfo a', asTypeInfo b') of
        (VTObject a, VTObject b) -> a `requireSubReftype` b
        (VTNull,     VTObject _) -> []
        (a,          b         )
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
                $ subclassPath hry (review _Binary s) (review _Binary t)
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


 -- where
 --   processByteCode :: B.ByteCodeOpr B.High -> State [B.VerificationTypeInfo B.High] [Fact]
 --   processByteCode = \case
 --     B.Put _ a -> undefined

 --     B.Invoke a -> case a of
 --       B.InvkSpecial _ ->
 --         undefined

payload ::
  Functor f =>
  p
  -> ((p, a) -> f (Maybe (p, a)))
  -> a -> f (Maybe a)
payload p fn a =
  fmap snd <$> fn (p, a)