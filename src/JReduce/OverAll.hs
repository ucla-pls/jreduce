-- |
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

-- text
import qualified Data.Text as Text

-- base
import Data.Maybe
import Data.Foldable
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

-- jvmhs
import Jvmhs.Data.Named
import Jvmhs.Data.Code
import Jvmhs.Transform.Stub

-- jvm-binary
import qualified Language.JVM.ByteCode as B

-- jreduce
import JReduce.Target

data Item
  = IContent Content
  | ICode (AbsMethodName, Code)
  | ITarget Target
  | ISuperClass (Class, ClassName)
  | IField (Class, Field)
  | IInnerClass (Class, InnerClass)

instance C.ToField ([Int], Item) where
  toField (i, x) =
    BL.toStrict . BS.toLazyByteString $ BS.stringUtf8 (show i) <> case x of
      IContent (Jar _) -> "jar"
      IContent (ClassFile c) ->
        BS.stringUtf8 (Text.unpack $ c^.className.fullyQualifiedName)
      IContent (MetaData _) -> "metadata"
      ICode (m, _) ->
        BS.stringUtf8 (Text.unpack $ absMethodNameToText m )
        <> "!code"
      ITarget _ -> "base"
      ISuperClass (c, _) ->
        BS.stringUtf8 (Text.unpack $ c^.className.fullyQualifiedName)
        <> "!superclass"
      IInnerClass (_, ic) ->
        BS.stringUtf8 (Text.unpack $ ic^.innerClass.fullyQualifiedName)
        <> "!isinner"
      IField (c, field) ->
        BS.stringUtf8
          . Text.unpack
          . absFieldNameToText
          $ mkAbsFieldName (c^.className) (field^.name)

makePrisms ''Item

data Fact
  = ClassExist ClassName
  | CodeIsUntuched AbsMethodName
  | HasSuperClass ClassName
  | FieldExist AbsFieldName
  | IsInnerClass ClassName
  deriving (Eq, Ord)

instance C.ToField Fact where
  toField = \case
    ClassExist _ -> "exist"
    CodeIsUntuched _ -> "unstubbed"
    HasSuperClass _ -> "hassuper"
    FieldExist _ -> "field"
    IsInnerClass _ -> "isinnerclass"

describeProblem :: MonadIO m => FilePath -> Problem a Target -> m (Problem a [IS.IntSet])
describeProblem wf p = do
  let p2 = liftProblem (review _ITarget) (fromJust . preview _ITarget) p

  liftIO
    . BL.writeFile (wf </> "graph.csv")
    . writeCSV
    . fst . reductionGraph keyFun itemR
    $ _problemInitial p2

  return (toGraphReductionDeep keyFun itemR p2)

classConstructors :: Fold Class AbsMethodName
classConstructors =
  classAbsMethodNames . filtered (elemOf methodId "<init>")

classInitializers :: Fold Class AbsMethodName
classInitializers =
  classAbsMethodNames . filtered (elemOf methodId "<clinit>")

keyFun :: Item -> (Maybe Fact, [Fact])
keyFun = \case
  IContent (ClassFile cls) ->
    ( Just (ClassExist $ cls ^.className)
    , concat
      [ (( classInterfaces . traverse
          <> classMethods . traverse . methodClassNames
          <> classBootstrapMethods . traverse . classNames
          <> classEnclosingMethod . _Just . (_1 <> _2 . _Just . classNames)
        ) . to ClassExist) `toListOf` cls
      , if cls ^. classAccessFlags . contains CEnum
        then cls ^.. classAbsFieldNames . to FieldExist
        else []
      , [ IsInnerClass (cls ^. className) ]
      ]
    )
    where
      methodClassNames =
        methodDescriptor . classNames
        <> methodExceptions . traverse
        <> methodSignature . _Just . classNames

  ISuperClass (cls, cn) ->
    ( Just (HasSuperClass $ cls^.className)
    , ClassExist cn : map CodeIsUntuched (toListOf classConstructors cls)
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

  IInnerClass (_, ic) ->
    ( Just (IsInnerClass $ ic ^. innerClass)
    , toListOf (classNames . to ClassExist) ic
    )

  -- IInterface cn ->
  --   ( Nothing, [KClassName cn] )

  ICode (m, code) ->
    ( Just (CodeIsUntuched m)
    , codeDependencies m code
    )

  _ -> (Nothing, [])

itemR :: PartialReduction Item Item
itemR f = \case
  ITarget t ->
    fmap ITarget <$> targetR f t
  IContent c ->
    fmap IContent <$> contentR f c
  a -> pure (Just a)
  where
    contentR :: PartialReduction Content Item
    contentR f' = \case
      ClassFile c -> fmap ClassFile <$> (part classR) f' c
      Jar c       -> fmap Jar <$> (deepDirForestR . reduceAs _IContent) f' c
      a           -> pure (Just a)

    targetR :: PartialReduction Target Item
    targetR = deepDirTreeR . reduceAs _IContent


payload ::
  Functor f =>
  p
  -> ((p, a) -> f (Maybe (p, a)))
  -> a -> f (Maybe a)
payload p fn a =
  fmap snd <$> fn (p, a)

classR :: Reduction Class Item
classR f c = do
  (super :: Maybe ClassName) <- case c ^. classSuper of
    Just "java/lang/Object" ->
      pure $ Just "java/lang/Object"
    Just a ->
       (payload c . reduceAs _ISuperClass) f a <&> \case
         Just a' -> Just a'
         Nothing -> Just "java/lang/Object"
    Nothing ->
      pure $ Nothing

  fields <-
    (listR . payload c . reduceAs _IField) f (c ^. classFields)

  methods <-
    (traverse . methodR (c^.className)) f (c ^. classMethods)

  innerClasses <-
    (listR . payload c . reduceAs _IInnerClass) f (c ^. classInnerClasses)

  -- interfaces <-
  --   (listR . reduceAs _IInterface) f (c ^. classInterfaces)

  pure $ c
    & classSuper .~ super
    & classFields .~ fields
    & classMethods .~ methods
    & classInnerClasses .~ innerClasses

methodR :: ClassName -> Reduction Method Item
methodR cn f m =
  case m ^. methodCode of
    Just c -> f (ICode (mkAbsMethodName cn (m ^. name), c)) <&> \case
      Just (ICode (_, c')) -> m & methodCode ?~ c'
      _ -> stub m
    _ -> pure m

codeDependencies :: AbsMethodName -> Code -> [Fact]
codeDependencies m = toListOf
  $ ( ( codeExceptionTable.folded.classNames
        <> codeStackMap._Just.classNames
        <> codeByteCode.folded.classNames
      )
      . to ClassExist
    )
  <> folding (const
              [ HasSuperClass (m^.inClassName)
              | m ^. methodId == "<init>"
              ])
  <> codeByteCode.folded.to B.opcode.folding processOpCode

  where
    processOpCode = \case
      B.Get _ f -> [FieldExist f]
      B.Put _ f -> [FieldExist f]
      _ -> []

 -- where
 --   processByteCode :: B.ByteCodeOpr B.High -> State [B.VerificationTypeInfo B.High] [Fact]
 --   processByteCode = \case
 --     B.Put _ a -> undefined

 --     B.Invoke a -> case a of
 --       B.InvkSpecial _ ->
 --         undefined
