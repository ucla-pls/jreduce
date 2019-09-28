-- |
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
module JReduce.OverStubs where

-- lens
import Control.Lens

-- jvmhs
import Jvmhs

-- containers
import qualified Data.IntSet as IS

-- base
import Data.Maybe

-- reduce-util
import Control.Reduce.Reduction
import Control.Reduce.Problem

-- jvmhs
import Jvmhs.Transform.Stub

-- jreduce
import JReduce.Target

data Item
  = IContent Content
  | ICode Code
  | ITarget Target

makePrisms ''Item

data Key
  = KClassName ClassName
  deriving (Eq, Ord)

describeProblem :: Problem a Target -> Problem a [IS.IntSet]
describeProblem =
  toGraphReductionDeep keyFun itemR
  . liftProblem (review _ITarget) (fromJust . preview _ITarget)

keyFun :: Item -> (Maybe Key, [Key])
keyFun = \case
  IContent (ClassFile cls) ->
    ( Just (KClassName $ cls ^.className)
    , map KClassName
      . toListOf
      ( classSuper . _Just . classNames
        <> classInterfaces . traverse . classNames
        <> classFields . traverse . classNames
        <> classMethods . traverse . methodClassNames
        <> classBootstrapMethods . traverse . classNames
        <> classEnclosingMethod . _Just . (_1 <> _2 . _Just . classNames)
        <> classInnerClasses . traverse . classNames
      )
      $ cls
    )
    where
      methodClassNames =
        methodDescriptor . classNames
        <> methodExceptions . traverse
        <> methodSignature . _Just . classNames


  ICode code ->
    ( Nothing
    , code ^.. classNames . to KClassName
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

classR :: Reduction Class Item
classR f c = do
  methods <-
    (traverse . methodR) f (c ^. classMethods)

  pure $ c & classMethods .~ methods

methodR :: Reduction Method Item
methodR f m =
  case m ^. methodCode of
    Just c -> f (ICode c) <&> \case
      Just (ICode c') -> m & methodCode ?~ c'
      _
        | m ^. methodId /= "<clinit>" && m ^. methodId /= "<init>" -> stub m
        | otherwise -> m
    _ -> pure m
