-- |
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module JReduce.OverClasses where

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

-- jreduce
import JReduce.Target

data Item
  = IContent Content
  | ITarget Target

type Key = ClassName

makePrisms ''Item

describeProblem :: Problem a Target -> Problem a [IS.IntSet]
describeProblem =
  toGraphReductionDeep keyFun itemR
  . liftProblem (review _ITarget) (fromJust . preview _ITarget)

keyFun :: Item -> (Maybe Key, [Key])
keyFun = \case
  IContent (ClassFile cls) ->
    ( Just (cls ^.className) , toListOf classNames cls )
  _ -> (Nothing, [])

itemR :: PartialReduction Item Item
itemR f = \case
  ITarget t ->
    fmap ITarget <$> targetR f t
  IContent c ->
    fmap IContent <$> contentR f c
  where
    contentR :: PartialReduction Content Item
    contentR f' = \case
      Jar c       -> fmap Jar <$> (deepDirForestR . reduceAs _IContent) f' c
      a           -> pure (Just a)

    targetR :: PartialReduction Target Item
    targetR = deepDirTreeR . reduceAs _IContent
