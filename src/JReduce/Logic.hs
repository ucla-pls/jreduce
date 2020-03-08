{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module JReduce.Logic where

-- lens
import Control.Lens hiding (andOf, orOf, (<.>))

-- containers
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.IntSet as IS

-- vector
import qualified Data.Vector as V

-- base
import Data.Foldable hiding (and, or)
import Data.Maybe
import Data.IORef
import Data.Tuple
import Data.Char (isNumber)
import Data.Monoid
import Text.Show
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.List as List
import           GHC.Generics                   ( Generic )
import Prelude hiding (fail, not, and, or)

-- jvmhs
import Jvmhs.Data.Type
import Jvmhs.TypeCheck
import Jvmhs.Data.Code
import Jvmhs hiding (methodExist, fieldExist)
import qualified Jvmhs 

-- containers
import qualified Data.IntMap.Strict            as IntMap

-- nfdata
import           Control.DeepSeq

-- jvm-binary
import qualified Language.JVM as B
import Language.JVM.ByteCode (ByteCodeOpr (..))

-- filepath
import System.FilePath

-- text
import qualified Data.Text as Text 
import qualified Data.Text.Lazy.IO as LazyText
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as LazyText
import qualified Data.Text.Lazy.Builder        as Builder

-- reduce-util
import Control.Reduce.Boolean
import Control.Reduce.Graph
import Control.Reduce.Boolean.CNF
import qualified Control.Reduce.Boolean.LiteralSet as LS
import Control.Reduce.Problem
import Control.Reduce.Reduction
import Control.Reduce.Util.Logger as L

-- unorderd-containers
import qualified Data.HashSet               as HS

-- jreduce
import JReduce.Target
import JReduce.Config


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

requireClassNamesOf ::
  (HasClassName c, HasClassNames a)
  => c -> Getting (Endo (Endo (Stmt Fact))) s a -> s -> Stmt Fact
requireClassNamesOf c l a =
  forallOf (l . classNames) a (requireClassName c)

decompose :: Ord a => S.Set a -> (M.Map a Int, V.Vector a)
decompose a =
  (M.fromAscList . toList $ imap (flip (,)) res, res)
  where res = V.fromList (toList a)

checkScope :: S.Set ClassName -> Fact -> Bool
checkScope scope = \case
  ClassExist     a           -> fn $ a
  CodeIsUntuched m           -> fn $ m ^. className
  HasSuperClass  cn _        -> fn $ cn
  HasInterface   cn _        -> fn $ cn
  FieldExist     f           -> fn $ f ^. className
  FieldIsFinal   f           -> fn $ f ^. className
  MethodExist    m           -> fn $ m ^. className
  IsInnerClass   cn _        -> fn $ cn
  -- MethodThrows   m _         -> fn $ m ^. className
  HasBootstrapMethod   cn _  -> fn $ cn
  Meta                       -> True
  where fn k = k `S.member` scope

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

      
unBuilder :: Builder.Builder -> String
unBuilder = LazyText.unpack . Builder.toLazyText

showsVariable :: V.Vector (Fact, [Int]) -> Int -> ShowS
showsVariable variables i = 
  case variables V.!? i of
    Just (fact, idx) -> 
      showString (unBuilder $ displayFact fact <> display (reverse idx))
    Nothing -> 
      shows i

initializeKeyFunction :: 
  forall m. MonadIOReader Config m 
  => LogicConfig -> Target -> FilePath -> m (V.Vector (Fact, [Int]), Int, ([Int], Item) -> m CNF)
initializeKeyFunction cfg trg wf = L.phase "Initializing key function" do
  lfn  <- logic cfg <$> fetchHierachy wf (targetClasses trg)
  core <- view cfgCore

  let 
    items = 
      (if reverseOrder cfg then reverse else id)
      (itemsOfTarget trg)
   
    factsToVar :: M.Map Fact (S.Set Int)
    factsToVar = 
      M.fromListWith S.union 
        [ (f, S.singleton i) 
        | (i, f) <- V.toList (V.indexed facts) 
        ]

    back :: V.Vector ([Int], Item)
    back = 
      V.fromList items
    
    facts :: V.Vector Fact
    facts =
      V.map (fst . lfn . snd) back

    variables :: V.Vector (Fact, [Int])
    variables = V.zip facts (V.map fst back)

    cores :: V.Vector Bool
    cores = 
      V.map (\fact-> serializeWith displayFact fact `HS.member` core) facts

    indiciesToVar :: M.Map [Int] Int
    indiciesToVar =
      M.fromList (map swap . V.toList . V.indexed . V.map fst $ back)
  
  maxid <- liftIO $ newIORef (V.length back)

  let
    handler :: ([Int], (Fact, Stmt Fact)) -> m CNF 
    handler (idx, (fact, stmt)) = L.logtime L.DEBUG ("Processing " <> debuginfo) $ do
      mid <- liftIO $ readIORef maxid 
      let cnf = toMinimalCNF mid nnfAfter
      liftIO $ writeIORef maxid 
        (max mid . maybe minBound fst . IS.maxView $ cnfVariables cnf)

      whenM (view cfgDumpItems) . liftIO $ do 
        LazyText.appendFile (wf </> "items.txt") . LazyText.toLazyText  
          $ displayText key <> display (reverse idx) <> "\n" 
        LazyText.appendFile (wf </> "items-logical.txt") . LazyText.toLazyText  
          $ displayText key <> display (reverse idx) <> " " <> display v <> "\n" 
          <> "  LV1 " <> displayString (showsStmtWith showsFact stmt "\n") 
          <> "  LV2 " <> displayString (showsNnfWith showsFact nnf "\n") 
          <> "  LV3 " <> displayString (showsNnfWith (showsVariable variables) nnfAfter "\n")
          <> foldMap (("  " <>) . displayClause) (cnfClauses cnf)

      return cnf
     where
      v   = indiciesToVar M.! idx
      key = serializeWith displayFact fact
      isCore = cores V.! v
      
      debuginfo = 
        displayText key <> (if isCore then " CORE" else "")

      nnf :: Nnf Fact
      nnf = 
        flattenNnf . nnfFromStmt . fromStmt $ stmt

      nnfAfter :: Nnf Int
      nnfAfter = 
        flattenNnf . nnfFromStmt . fromStmt 
        . (case idx of 
            [] -> id 
            _:rest -> \s -> 
              s /\ (tt (indiciesToVar M.! idx) ==> tt (indiciesToVar M.! rest))
          )
        . (if not isCore then id else \s -> s /\ tt (indiciesToVar M.! idx))
        . runIdentity
        $ traverseVariables (\f -> pure $ forallOf (ix f.folded) factsToVar \i -> tt i
        ) stmt

      showsFact :: Fact -> ShowS
      showsFact = showString . unBuilder . displayFact

      displayClause c = 
        displayString (LS.displayImplication (showsVariable variables) c "\n")

  L.info . L.displayf "Found %d items." $ List.length items
  L.info . L.displayf "Found %d facts." $ M.size factsToVar
  L.info . L.displayf "The core is %d of them." $ List.length core

  mid <- liftIO $ readIORef maxid
  return (variables, mid, handler . over _2 lfn) 

 where 
  itemsOfTarget :: Target -> [([Int], Item)]
  itemsOfTarget = itoListOf (deepSubelements itemR) . review _ITarget

computeCNF :: 
  MonadIOReader Config m 
  => (Int -> ShowS) 
  -> (a -> m CNF) 
  -> FilePath 
  -> [a] 
  -> m CNF
computeCNF sv keyFun wf items = L.phase "Compute CNF" do 
  cnf <- CNF . foldMap cnfClauses <$> mapM keyFun items
  whenM (view cfgDumpLogic) . liftIO $ do
    LazyText.appendFile (wf </> "cnf.txt") 
    . LazyText.toLazyText  
    $ foldMap (\c -> displayString $ LS.displayImplication sv
               c "\n") (cnfClauses cnf)
  return cnf

describeLogicProblem :: 
  forall a m.
  MonadIOReader Config m
  => LogicConfig
  -> FilePath 
  -> Problem a Target
  -> m (IPF, Problem a IS.IntSet)
describeLogicProblem cfg wf p = flip refineProblemA' p \s -> do
  (variables, _, keyFun) <- initializeKeyFunction cfg s wf
  cnf <- computeCNF (showsVariable variables) keyFun wf
    $ itoListOf (deepSubelements itemR) (ITarget s)

  let 
    ipf = case fromCNF $ cnf of
      Just ipf' -> ipf'
      Nothing  -> error "The created CNF was not IPF"

    fromVars :: IS.IntSet -> Maybe Target
    fromVars vars = preview _ITarget =<<
      limit (deepReduction itemR) (`S.member` varset) (ITarget s)
      where 
        varset = S.fromList . map snd 
          . mapMaybe (variables V.!?) 
          . IS.toList 
          $ vars

    costfn =
      fromIntegral . IS.size . fst . IS.split (V.length variables)

  dumpCore <- view cfgDumpCore 
  dumpClosures <- view cfgDumpClosures
  when (dumpCore || dumpClosures) . liftIO $ do
    let (core, progress) = weightedProgression costfn ipf
    when dumpCore do
      LazyText.writeFile (wf </> "core.txt") . LazyText.toLazyText  
        $ foldMap (\a -> displayShowS (showsVariable variables a) <> "\n") (IS.toList core)

    when dumpClosures do
      LazyText.writeFile (wf </> "progression.txt") . LazyText.toLazyText  
        . foldMap 
          (\a -> (fold 
            . List.intersperse ", " 
            . map (displayShowS . showsVariable variables)
            $ IS.toList a) 
            <> "\n") 
        $ progress

  return 
    ( ipf
    , (fromVars, ipfVars ipf)
    )

approxLogicProblem :: 
  IPF 
  -> Problem a IS.IntSet
  -> Problem a [Int]
approxLogicProblem ipf = 
  liftProblem IS.toList (resolver . IS.fromList)
  where resolver = fasterLWCC ipf 


displayShowS :: ShowS -> Builder.Builder
displayShowS f = displayString (f "")

describeGraphProblem ::
  MonadIOReader Config m
  => LogicConfig
  -> FilePath
  -> Problem a Target
  -> m (Problem a [IS.IntSet])
describeGraphProblem cfg wf p = flip refineProblemA p \s -> do
  (variables, mid, keyFun) <- initializeKeyFunction cfg s wf
  cnf <- computeCNF (showsVariable variables) keyFun wf
    $ itoListOf (deepSubelements itemR) (ITarget s)

  let 
    (required, edges') = fold 
      [ case over both IS.minView $ LS.splitLiterals clause of 
          (Nothing    , Just (t, _)) -> (IS.singleton t, mempty)
          (Just (f, _), Just (t, _)) -> (mempty, S.singleton (f,t))
          _                          -> error "CNF is not IPF"
      | clause <- S.toList $ cnfClauses cnf 
      ]

    (graph, rev) = buildGraphFromNodesAndEdges 
      [(k,k) | k <- [0..mid - 1]] 
      [Edge () f t | (f, t) <- S.toList edges']

    core = closure graph (mapMaybe rev $ IS.toList required)

    fromClosures cls = preview _ITarget =<<
      limit (deepReduction itemR) (`S.member` varset) (ITarget s)
      where
        varset = S.fromList . map snd 
          . mapMaybe (variables V.!?) 
          . map (nodeLabel . (nodes graph V.!))
          . IS.toList . IS.unions
          $ core:cls

    _targets =
      filter (not . IS.null)
      . map (IS.\\ core)
      $ closures graph
    
  dumpGraphInfo wf 
    (graph <&> flip (showsVariable variables) "")
    core _targets

  -- ( fromIntegral . IS.size . fst 
  --     . IS.split (V.length variables) 
  --     . IS.unions
  return (fromClosures, _targets)


logic :: LogicConfig -> Hierarchy -> Item -> (Fact, Stmt Fact)
logic LogicConfig{..} hry = \case

  IContent (ClassFile cls) -> ClassExist (cls ^. className)
    `withLogic` \c ->
    [ -- We also do not reduce type parameters. Thier requirements are just that
      -- all classes mention should exist if this class exist.
      c ==> requireClassNamesOf cls (classTypeParameters.folded) cls

    , c ==> requireClassNamesOf cls (classAnnotations.folded) cls

    , -- If the class is a enum, it needs to extend java.lang.Enum and have 
      -- these methods and fields
      given (cls^.classAccessFlags.contains CEnum) $ c ==> 
        requireSubclass hry (cls^.className) "java/lang/Enum"
        /\ given (cls^?classSuper._Just.simpleType == Just "java/lang/Enum") 
        ( and 
          [ requireMethod hry cls . mkAbsMethodId cls $ "values" 
             <:> MethodDescriptor [] 
              (ReturnDescriptor . Just . JTRef . JTArray .JTRef . JTClass $ cls^.className)
          , requireMethod hry cls . mkAbsMethodId cls $ "valueOf" 
            <:> MethodDescriptor ["Ljava/lang/String;"] 
              (ReturnDescriptor . Just . JTRef . JTClass $ cls^.className)
          , requireField hry cls . mkAbsFieldId cls $ "$VALUES" 
            <:> FieldDescriptor  (JTRef . JTArray .JTRef . JTClass $ cls^.className)
          ]
        )
    , -- We also do also not reduce enclosing methods. If a class is enclosed
      -- in another class, require that to exist, and if the class is enclosed
      -- in a method require that to exist.
      forallOf (classEnclosingMethod._Just) cls
      \(cn, mMId) -> c ==> case mMId of
        Just m -> codeIsUntuched (mkAbsMethodId cn m) /\ isInnerClassOf cls cn
        Nothing -> requireClassName cls cn
    ]

  IField (cls, field) -> FieldExist (mkAbsFieldId cls field)
    `withLogic` \f ->
    [ f ==> requireClassNamesOf cls fieldType field
    , f ==> requireClassNamesOf cls (fieldAnnotations.folded) field
    , -- TODO: Reconsider this?
      -- If any field is synthetic we will require it to not be removed, if the
      -- class exist. This helps with many problems.
      given (FSynthetic `S.member` flags) do
        classExist cls ==> f 

    , -- If class is an interface and the feild is static keep the 
      -- classInitializers
      given (cls^.classAccessFlags .contains CAbstract && field^.fieldAccessFlags.contains FStatic) do 
        forallOf classInitializers cls \m ->
          f ==> codeIsUntuched m
    ]
    where flags = field^.fieldAccessFlags
  
  IFieldFinal (cls, field) -> FieldIsFinal (mkAbsFieldId cls field)
    `withLogic` \f ->
    [ -- If a field is final it has to be set. This means we cannot stub
      -- class initializers and class constructors.
      if FStatic `S.member` flags
      then
        forallOf classInitializers cls \m -> 
          f ==> codeIsUntuched m
      else
        forallOf classConstructors cls \m -> 
          f ==> codeIsUntuched m
    , -- If a field is synthetic or static do not remove any final flags.
      -- final static fields are treated differently than other fields, and 
      -- are more like constants.
      given (FSynthetic `S.member` flags  \/ FStatic `S.member` flags) $
        fieldExist (mkAbsFieldId cls field) ==> f
    ]
    where flags = field^.fieldAccessFlags

  IMethod (cls, method) -> MethodExist (mkAbsMethodId cls method)
    `withLogic` \m ->
    [ -- Since we do not remove the return argument or the arguemnts we have to build
      -- their requirements here.
      m ==> requireClassNamesOf cls 
        (methodReturnType.classNames <> methodParameters.folded.classNames) 
        method

    , -- If you are a constructor, you have to be removed completely if you can be 
      -- removed
      given (method^.methodIdName == "<init>") $ 
        m ==> codeIsUntuched (mkAbsMethodId cls method)

      -- Require the classNames of the exceptions
    , m ==> requireClassNamesOf cls (methodExceptions.folded) method

    , -- Type parameters might contain classes
      m ==> requireClassNamesOf cls
        (methodTypeParameters.folded)
        method
    
    , m ==> requireClassNamesOf cls 
        (methodAnnotations.folded) 
        method

    , if method^.methodAccessFlags.contains MAbstract
      then
      -- If a method is abstract, then it has to be implemented in all of its
      -- implementations. Say that A implements I and is not abstract, then
      -- it should implement all the methods of I. For each such path, we
      -- require it to be true or one of it's super classes to have implemented
      -- it.
      forall (implementationPaths (cls^.className) hry)
        \(def, isAbstract, path) ->
          given (not isAbstract)
          $ m /\ unbrokenPath path
          ==> requireNonAbstractMethod hry cls (mkAbsMethodId def method)
      else
      -- If the methods is not abstract, make sure that the method defintion
      -- does exist. A chain from A <: I <: !I. If I does not exit, either
      -- this method have to stay or we have to remove the implements interface.
      forall (superDeclarationPaths (mkAbsMethodId cls method) hry)
        \(decl, isAbstract, path) -> given isAbstract
          $ methodExist decl /\ unbrokenPath path ==> 
            requireNonAbstractMethod hry cls (mkAbsMethodId cls method)

    , m ==> requireClassNamesOf cls (methodDefaultAnnotation._Just) method

    , -- TODO: Nessary?
      -- Finally we require that if a method is synthetic is should be
      -- removed alongside its code
      given (method^.methodAccessFlags.contains MSynthetic)
      $ m ==> codeIsUntuched (mkAbsMethodId cls method)
    ]

  IImplements (cls, ct) -> HasInterface (cls^.className) (ct^.simpleType)
    `withLogic` \i ->
    [ -- An Implements only depends on the interface that it implements, and
      -- its type parameters.
      i ==> requireClassNames cls ct
    , -- Given that we should keep the extends
      given keepHierarchy $ classExist cls ==> i
    ]

  ISuperClass (cls, ct) -> HasSuperClass (cls^.className) (ct^.simpleType)
    `withLogic` \s ->
    [ -- An Implements only depends on the class of the supertype and its type
      -- parameters.
      s ==> requireClassNames cls ct
      
    , -- In case the superclass have no empty init method we require at least
      -- one of it's constructors to exist.
      let 
        ctc = ct^.simpleType
        mid = mkAbsMethodId ctc ("<init>:()V" :: MethodId)
      in 
      s ==> 
        case Jvmhs.methodExist mid hry of 
          Just (view stubMethodAccess -> access) 
            |  access >= Protected 
            \/ access == Default /\  ctc^.package == cls^.className.package ->
              methodExist mid \/ existOf classConstructors cls codeIsUntuched
          _ -> 
            existOf classConstructors cls codeIsUntuched

    , -- Given that we should keep the extends
      given keepHierarchy $ classExist cls ==> s
    ]

  IInnerClass (cls, ic) -> IsInnerClass (cls^.className) (ic^.innerClass)
    `withLogic` \i ->
    [ -- An innerclass depends on all classes referenced by the innerClass.
      i ==> requireClassNames cls ic

    , -- If inner class is ponting to itself, then it required as long at the
      -- class exist.
      given (cls^.className == ic^.innerClass) $
      classExist cls ==> i

    , -- If the outer class is an this class then we can not remove this 
      -- innerclass statement before the innerclass have been removed
      given (Just (cls^.className) == ic^.innerOuterClass) $
      classExist (ic^.innerClass) ==> i

      -- NOTE: That all requirements that a class exist also will check if
      -- the innerclass exist. The rule is that if a class refer to an innerclass
      -- it must have an innerclass entry that describes that class.
    ]

  IBootstrapMethod (cls, (i, btm)) -> 
    HasBootstrapMethod (cls ^.className) i `withLogic` \bm -> 
    [ bm ==> requireMethodHandle hry cls (btm^.bootstrapMethodHandle)
    , bm ==> forallOf (bootstrapMethodArguments.folded) btm \case 
        VClass rf -> requireClassNames cls rf
        VMethodType md -> 
          -- We would love to require the method, but we do not know the
          -- abslocatio of the MethodDescriptor
          requireClassNames cls md
        VMethodHandle mh -> 
          requireMethodHandle hry cls mh
        _ -> true
    ] 
    -- -- We do currently not reduce bootstrap methods, so their requirements are
    --   -- handled from here.
    --   forallOf (classBootstrapMethods.folded) cls
    --     \(BootstrapMethod mhandle args) ->
    --       c ==> requireMethodHandle hry cls mhandle
    --       /\ requireClassNamesOf cls folded args

    -- , ]

  -- IMethodThrows ((cls, method), mt) ->
  --   MethodThrows (mkAbsMethodId cls method)
  --   (mt^.simpleType)
  --   `withLogic` \m ->
  --   [ -- A method throws statement depends on all the class it mentions.
  --     m ==> requireClassNames cls mt

  --   , -- TODO: An indepth analysis of throws of the code?
  --     given (has (methodCode._Just) method) $
  --       codeIsUntuched (mkAbsMethodId cls method) ==> m

  --   , -- Any class mentioned in this setting should extend throwable.
  --     m ==> mt^.simpleType `requireSubtype` ("java/lang/Throwable" :: ClassName)

  --   -- , -- TODO: I this method extends a method it has to have it's execeptions.
  --   --   forall (superDeclarationPaths mt hry)
  --   --     \(decl, isAbstract, path) -> given isAbstract
  --   --       $ methodExist decl /\ unbrokenPath path ==> m

  --   ]

  ICode ((cls, method), code) -> CodeIsUntuched theMethodName
    `withLogic` \c ->
    [ -- If the code was not stubbed, then we have to require that the
      -- classes in the exception table, stack map, and byte-code instructions
      -- exits
     -- c ==> requireClassNamesOf cls (codeExceptionTable.folded) code
      c ==> requireClassNamesOf cls (codeStackMap._Just) code
    , c ==> requireClassNamesOf cls (codeByteCode.folded) code
    , c ==> forallOf (codeExceptionTable.folded.ehCatchType._Just) code \ct -> 
        requireClassName cls ct
        /\ ct `requireSubtype` ("java/lang/Throwable" :: ClassName)
      
    ] ++
    [ case oper of
        ArrayStore _ ->
          -- When we store an item in the array, it should be a subtype of the
          -- content of the array.
          c ==> stack 0 `requireSubtype` isArray (stack 2)

        Get fa fid ->
          -- For a get value is valid the field has to exist, and the first
          -- element on the stack has to be a subclass of fields class.
          c ==> requireField hry cls fid
            /\ given (fa /= B.FldStatic) (stack 0 `requireSubtype` fid^.className)
       
        -- TODO: Experimental overapproximation. 
        -- The idea is to require all extensions of the class variable.
        Push (Just (VClass (JTClass cn))) ->
          c ==> forall (S.fromList p') unbrokenPath
         where 
          p'= 
           [ path 
           | b <- superclasses cn hry 
           , path <- subclassPaths cn b hry 
           ]

        Put fa fid ->
          -- For a put value is valid the field has to exist, and the first
          -- element on the stack has to be a subclass of fields class, and
          -- the second element have to be a subtype of the type of the field
          c ==> requireField hry cls fid
            /\ stack 0 `requireSubtype` fid^.fieldIdType
            /\ given (fa /= B.FldStatic)
              (stack 1 `requireSubtype` fid^.className)

        Invoke a ->
          -- For the methods there are three general cases, a regular method call,
          -- a static methods call (no-object) and a dynamic call (no-class).
          methodRequirements
          /\ (c ==> and 
              [ s `requireSubtype` t 
              | (s, t) <- zip (state ^. tcStack) (reverse stackTypes)
              ]
            )
          where
            (methodRequirements, stackTypes) =
              case methodInvokeTypes a of
                Right (isSpecial, isStatic, m) ->
                  ( let mid = AbsMethodId $ m^.asInClass
                    in (c ==> if isSpecial then methodExist mid else requireMethod hry cls mid)
                        /\ given (Text.isPrefixOf "access$" (m^.methodIdName))
                          (methodExist mid ==> c)   
                        /\ given (
                            ( maybe False (isNumber . fst) . Text.uncons . last . Text.splitOn "$" 
                            $ mid^.className.fullyQualifiedName
                            ) 
                            /\ mid^.className /= cls ^.className) 
                          (classExist (mid^.className) ==> c)
                  , [asTypeInfo $ m^.asInClass.className | not isStatic]
                    <> (map asTypeInfo $ m^.methodIdArgumentTypes)
                  )
                Left (i, m) -> 
                  ( ( c ==> requireBootstrapMethod cls (fromIntegral i) ) 
                    /\ ( requireBootstrapMethod cls (fromIntegral i) ==> c)
                  -- BootstrapMethods are bound to thier use without them
                  -- they are nothing and should be removed 
                  , map asTypeInfo $ m^.methodIdArgumentTypes
                  )

        Throw ->
          -- A Throw operation requires that the first element on the stack is throwable.
          c ==> stack 0 `requireSubtype` ("java/lang/Throwable" :: ClassName)

        CheckCast fa ->
          -- The check cast operation requires that the first element on the stack
          -- is either a subtype of the cast or the cast is a subtype of the first
          -- element. Often only one of these are true.
          c ==> stack 0 `requireSubtype` fa \/ fa `requireSubtype` stack 0

        Return (Just B.LRef) ->
          -- We do require that the first element on the stack is a subtype of the return type.
          c ==> forall (method^.methodReturnType.simpleType)
            \mt -> stack 0 `requireSubtype` mt

        InstanceOf ct ->
          c ==> ct `requireSubtype` stack 0

        _ -> true
    | (state, B.opcode -> oper) <-
        V.toList $ V.zip typeCheckStates (code ^. codeByteCode)
    , let stack n =
            case state ^? tcStack.ix n of
              Just a -> a
              Nothing ->
                error $
                "Incompatable stack length: " <> show n
                <> " at: " <> show theMethodName
                <> " bc: " <> show oper
                <> " current stack: " <> show (state^.tcStack)
    ]
    where
      theMethodName =
        mkAbsMethodId cls method

      methodInvokeTypes = \case
        B.InvkSpecial (B.AbsVariableMethodId _ m) -> Right (True, False, m)
        B.InvkVirtual m -> Right (False, False, m)
        B.InvkStatic (B.AbsVariableMethodId _ m) -> Right (False, True, m)
        B.InvkInterface _ (B.AbsInterfaceMethodId m) -> Right (False, False, m)
        B.InvkDynamic (B.InvokeDynamic i m') -> Left (i, m')

      typeCheckStates =
        case typeCheck hry theMethodName (method^.methodAccessFlags.contains MStatic) code of
          (Just (i, x), _) -> error
            (show theMethodName
              ++ " "
              ++ show (code^?codeByteCode.ix i)
              ++ " "
              ++ show x)
          (Nothing, vc) -> vc

  IContent (Jar _) -> (Meta, true)
  IContent (MetaData _) -> (Meta, true)
  ITarget _ -> (Meta, true)

  where

    infixl 6 `requireSubtype`
    requireSubtype ::
      (AsTypeInfo a, AsTypeInfo b)
      => a -> b
      -> Stmt Fact
    requireSubtype (asTypeInfo -> TRef as) (asTypeInfo -> TRef bs) = and
      [ a `requireSubRefType` b | a <- toList as, b <- toList bs]
      where
        requireSubRefType a b = case a of
          B.JTClass s -> case b of
            B.JTClass "java/lang/Object" -> true
            B.JTClass t -> and
              [ unbrokenPath path
              | path <- subclassPaths s t hry
              ]
            _ -> true
          B.JTArray (JTRef s) -> case b of
            B.JTArray (JTRef t) -> s `requireSubRefType` t
            _ -> true
          _ -> true
    requireSubtype _ _ = true

    -- Return the type of array execpt if it the typeinfo is null in which case
    -- we return Nothing
    isArray :: TypeInfo -> TypeInfo
    isArray ti =
      fromJust $
      foldl (\a b -> a >>= meet (asTypeInfo b))
      (Just TTop)
      (ti ^.._TRef.folded._JTArray)

unbrokenPath :: SubclassPath -> Stmt Fact
unbrokenPath path =
  and [ isSubclass f t e | (f, t, e) <- subclassEdges path]

isSubclass :: ClassName -> ClassName -> HEdge -> Stmt Fact
isSubclass cn1 cn2 = \case
  Implement -> hasInterface cn1 cn2
  Extend -> hasSuperClass cn1 cn2

requireSubclass :: Hierarchy -> ClassName -> ClassName -> Stmt Fact
requireSubclass hry s t = 
  case t of 
    "java/lang/Object" -> true
    _ -> and [ unbrokenPath path | path <- subclassPaths s t hry ]

hasInterface :: ClassName -> ClassName -> Stmt Fact
hasInterface cn1 cn2 = tt (HasInterface cn1 cn2)

hasSuperClass :: ClassName -> ClassName -> Stmt Fact
hasSuperClass cn1 cn2 = tt (HasSuperClass cn1 cn2)

requireMethodHandle :: HasClassName c => Hierarchy -> c -> B.MethodHandle B.High -> Stmt Fact
requireMethodHandle hry cls = \case
  B.MHField (B.MethodHandleField _ f)
    -> requireField hry cls f
  B.MHMethod a -> requireMethod hry cls . AbsMethodId . view asInClass $ case a of
    B.MHInvokeVirtual rt -> rt
    B.MHInvokeStatic (B.AbsVariableMethodId _ rt) -> rt
    B.MHInvokeSpecial (B.AbsVariableMethodId _ rt) -> rt
    B.MHNewInvokeSpecial rt -> rt
  B.MHInterface (B.MethodHandleInterface (B.AbsInterfaceMethodId rt)) ->
    requireMethod hry cls . AbsMethodId . view asInClass $ rt

requireBootstrapMethod :: HasClassName c => c -> Int -> Stmt Fact
requireBootstrapMethod c i = tt (HasBootstrapMethod (c^.className) i)

requireClassNames :: (HasClassName c, HasClassNames a) => c -> a -> Stmt Fact
requireClassNames c =
  andOf (classNames . to (requireClassName c))

requireClassName :: (HasClassName c, HasClassName a) => c -> a -> Stmt Fact
requireClassName oc ic =
  classExist ic /\ isInnerClassOf oc ic

classExist :: HasClassName a =>  a -> Stmt Fact
classExist (view className -> cn) =
  tt (ClassExist cn)

fieldExist :: AbsFieldId -> Stmt Fact
fieldExist f =
  tt (FieldExist f)

methodExist :: AbsMethodId -> Stmt Fact
methodExist f =
  tt (MethodExist f)


orFailWith :: String -> [Stmt a] -> Stmt a
orFailWith f = \case
  [] -> error f
  a:as -> foldr (\/) a as

requireField :: HasClassName c => Hierarchy -> c -> AbsFieldId -> Stmt Fact
requireField hry cn fid = isInnerClassOf cn fid /\ orFailWith ("Could not find " ++ show fid)
  [ fieldExist fid' /\ unbrokenPath path
  | (fid', path) <- fieldLocationPaths fid hry
  ]

requireMethod :: HasClassName c => Hierarchy -> c -> AbsMethodId -> Stmt Fact
requireMethod hry cn mid = isInnerClassOf cn mid /\ orFailWith ("Could not find " ++ show mid)
  [ methodExist mid' /\ unbrokenPath path
  | (mid', _, path) <- superDeclarationPaths mid hry
  ]

requireNonAbstractMethod :: HasClassName c => Hierarchy -> c -> AbsMethodId -> Stmt Fact
requireNonAbstractMethod hry cn mid = isInnerClassOf cn mid /\ orFailWith ("Could not find " ++ show mid)
  [ methodExist mid' /\ unbrokenPath path
  | (mid', False, path) <- superDeclarationPaths mid hry
  ]

codeIsUntuched :: AbsMethodId -> Stmt Fact
codeIsUntuched m =
  tt (CodeIsUntuched m)

isInnerClassOf :: (HasClassName c1, HasClassName c2) => c1 -> c2 -> Stmt Fact
isInnerClassOf (view className -> c1) (view className -> c2) =
   given (isInnerClass c2) (tt (IsInnerClass c1 c2))

withLogic :: Fact -> (Stmt Fact -> [Stmt Fact]) -> (Fact, Stmt Fact)
withLogic f fn = (f, and (fn (tt f)))

whenM :: Monad m => (m Bool) -> m () -> m ()
whenM mb m = mb >>= \b -> when b m

classConstructors :: Fold Class AbsMethodId
classConstructors = classAbsMethodIds . filtered (elemOf methodIdName "<init>")

classInitializers :: Fold Class AbsMethodId
classInitializers =
  classAbsMethodIds . filtered (elemOf methodIdName "<clinit>")

payload :: Functor f => p -> ((p, a) -> f (Maybe (p, a))) -> a -> f (Maybe a)
payload p fn a = fmap snd <$> fn (p, a)

methodIsAbstract :: Method -> Bool
methodIsAbstract = view (methodAccessFlags . contains MAbstract)
