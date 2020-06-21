-- |
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module JReduce.Classes where

-- base
import Control.Monad
import Data.Tuple
import Data.Maybe
import Prelude hiding (and)
import Data.Foldable hiding (and)

-- lens
import Control.Lens

-- jvmhs
import Jvmhs

-- containers
import qualified Data.IntSet as IS
import qualified Data.Set as S
import qualified Data.Map.Strict as M

-- text
import Data.Text.Lazy.Builder

-- vector
import qualified Data.Vector as V

-- reduce-util
import Control.Reduce.Reduction
import Control.Reduce.Problem
import Control.Reduce.Boolean.CNF
import Control.Reduce.Boolean
import qualified Control.Reduce.Graph as G
import qualified Control.Reduce.Boolean.LiteralSet as LS

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
  deriving (Eq, Ord)

makePrisms ''Item

describeProblem ::
  MonadIOReader Config m
  => FilePath
  -> Problem a Target
  -> m (Problem a [IS.IntSet])
describeProblem = describeProblemTemplate
  itemR (pure $ \i -> let (k,ks) = keyFun i in pure (k, map (k,) ks))
  displayKey _ITarget

-- Calculate a reduction graph from a key function.
reductionGraph ::
  (Ord k)
  => (s -> (k, [k]))
  -- ^ A key function
  -> PartialReduction s s
  -- ^ A partial reduction
  -> s
  -> ( G.Graph () ([Int], k), [Int] -> Maybe G.Vertex)
reductionGraph keyfn red s = G.buildGraphFromNodesAndEdges
  [ (n, (n, k)) | (n, k, _) <- nodes_ ]
  edges_
 where
  nodes_ =
    [ (i, mk, ks)
    | (i, a) <- itoListOf (deepSubelements red) s
    , let (mk, ks) = keyfn a
    ]

  keymap =
    M.fromListWith S.union
    [ (k, S.singleton n)
    | (n, k, _) <- nodes_
    ]

  edges_ =
    concat
    [ addInit n
      [ G.Edge () f_ t_
      | t <- ks
      , f_ <- S.toList . fold $ keymap M.!? k
      , t_ <- S.toList . fold $ keymap M.!? t
      ]
    | (n, k, ks) <- nodes_
    ]
  addInit = (\case [] -> id; a -> (G.Edge () a (tail a) :))


describeLogicProblem ::
  MonadIOReader Config m
  => FilePath
  -> Problem a Target
  -> m (CNF, V.Vector (Key, [Int]), Problem a IS.IntSet)
describeLogicProblem wf p = (\((a,b), c) -> (a,b,c)) <$>
    flip refineProblemA' p \s -> do
  let
    (graph, _) = reductionGraph keyFun itemR (ITarget s)

    cnf = CNF
      . S.fromList
      $ [ LS.fromList' [ ff f,  tt t ] | G.Edge () f t <- G.edges graph ]

    fromVars :: IS.IntSet -> Maybe Target
    fromVars vars = preview _ITarget =<<
      limit (deepReduction itemR) (`S.member` varset) (ITarget s)
      where
        varset = S.fromList
          . map (\i -> fst (G.nodeLabels graph V.! i ))
          . IS.toList
          $ vars

  return
    ( (cnf, V.map swap $ G.nodeLabels graph)
    , (fromVars, cnfVariables cnf)
    )
  -- describeProblemTemplate
  -- itemR (pure $ \i -> let (k,ks) = keyFun i in pure (k, map (k,) ks))
  -- displayKey _ITarget

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

