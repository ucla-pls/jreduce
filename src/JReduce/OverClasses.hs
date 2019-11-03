-- |
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module JReduce.OverClasses where

-- lens
import Control.Lens

-- jvmhs
import Jvmhs

-- containers
import qualified Data.IntSet as IS

-- text
import Data.Text.Lazy.Builder

-- reduce-util
import Control.Reduce.Reduction
import Control.Reduce.Problem

-- jreduce
import JReduce.Target
import JReduce.Config

data Item
  = IContent Content
  | ITarget Target

data Key
  = KClass ClassName
  | KJar
  | KMeta
  | KBase

makePrisms ''Item

describeProblem ::
  MonadIOReader Config m
  => FilePath
  -> Problem a Target
  -> m (Problem a [IS.IntSet])
describeProblem = describeProblemTemplate
  itemR (pure $ \i -> let (k,ks) = keyFun i in pure (k, map (k,) ks))
  displayKey _ITarget

keyFun :: Item -> (Key, [Key])
keyFun = \case
  IContent (ClassFile cls) ->
    ( KClass (cls ^.className) , map KClass $ toListOf classNames cls)
  IContent (Jar _ ) ->
    ( KJar , [])
  IContent (MetaData _ ) ->
    ( KMeta , [])
  ITarget _ ->
    ( KBase , [])

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


displayKey :: Key -> Builder
displayKey = \case
  KJar -> "jar"
  KClass cn -> toBuilder cn
  KMeta -> "meta"
  KBase -> "base"
