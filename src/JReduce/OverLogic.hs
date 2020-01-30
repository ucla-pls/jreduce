{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module JReduce.OverLogic where

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
import Data.Char (isNumber)
import Data.Bool (bool)
import Data.Monoid
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.List as List
import Prelude hiding (fail, not, and, or)

-- jvmhs
import Jvmhs.Data.Type
import Jvmhs.TypeCheck
import Jvmhs.Data.Code
import Jvmhs hiding (methodExist, fieldExist)
import qualified Jvmhs 

-- nfdata
import           Control.DeepSeq

-- jvm-binary
import qualified Language.JVM as B
import Language.JVM.ByteCode (ByteCodeOpr (..))

-- filepath
import System.FilePath

-- -- directory
-- import System.Directory

-- text
import qualified Data.Text as Text 
import qualified Data.Text.Lazy.IO as LazyText
import qualified Data.Text.Lazy.Builder as LazyText

-- reduce-util
import Control.Reduce.Boolean
import Control.Reduce.Problem
import Control.Reduce.Reduction
import Control.Reduce.Util.Logger as L

-- unorderd-containers
import qualified Data.HashSet               as HS

-- jreduce
import JReduce.Target
import JReduce.Config
import JReduce.OverDeep ( Item (..), Fact (..)
                        , classInitializers, classConstructors
                        , itemR, displayFact
                        , _ITarget
                        )

requireClassNamesOf ::
  (HasClassName c, HasClassNames a)
  => c -> Getting (Endo (Endo (Term Fact))) s a -> s -> Term Fact
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
  MethodExist    m           -> fn $ m ^. className
  IsInnerClass   cn _        -> fn $ cn
  -- MethodThrows   m _         -> fn $ m ^. className
  HasBootstrapMethod   cn _  -> fn $ cn
  Meta                       -> True
  where fn k = k `S.member` scope

describeGraphProblem ::
  MonadIOReader Config m
  => Bool 
  -- ^ Keep hierarchy
  -> Bool
  -- ^ Is overapproximation 
  -> FilePath
  -> Problem a Target
  -> m (Problem a [IS.IntSet])
describeGraphProblem hier isOver wf p = do
  -- describeProblemTemplate itemR genKeyFun displayFact _ITarget wf p
  let p2 = liftProblem (review _ITarget) (fromJust . preview _ITarget) p
  (keyFun, _) <- L.phase "Initializing key function" $ do
      let targets = targetClasses $ _problemInitial p
      let scope = S.fromList ( map (view className) targets)
      hry <- fetchHierachy targets
      pure . (,scope) $ pure . logic (LogicConfig { keepHierarchy = hier }) hry

  L.phase "Precalculating the Reduction" $ do
    core <- view cfgCore
    L.info . L.displayf "Requiring %d core items." $ List.length core

    removeables :: S.Set Fact <- S.fromList 
      <$> mapM 
        (fmap fst . keyFun) 
        (toListOf (deepSubelements itemR) (_problemInitial p2))

    ((grph, coreSet, closures), p3) <- 
      toGraphReductionDeepM (handler removeables core <=< keyFun) itemR p2
    
    dumpGraphInfo wf (grph <&> over _2 (serializeWith displayFact)) coreSet closures
   
    return p3

 where 
  -- handler :: S.Set Fact -> HS.HashSet Text.Text -> (Fact, Term Fact) -> m (Fact, Bool, [(Fact, Fact)])
  handler removeables core (key, sentence) = do
    let txt = serializeWith displayFact key
        isCore = txt `HS.member` core

    let debuginfo = displayText txt 
              <> (if isCore then " CORE" else "")

    -- Ensure that the sentence have been evalutated
    L.logtime L.DEBUG ("Processing " <> debuginfo) $ deepseq sentence (pure ())

    let nnf = flattenNnf . nnfFromTerm . fromTerm . runIdentity  $ traverseVariables 
          (\n -> if n `S.member` removeables
            then
              pure $ tt n  
            else do
              -- L.warn ("Did not find " <> displayFact n)
              pure $ true
          )
          sentence

    let edges = if isOver
          then overDependencies nnf
          else underDependencies nnf

    whenM (view cfgDumpItems) . liftIO $ do
      let filename = wf </> "items.txt" 
      LazyText.appendFile filename . LazyText.toLazyText  
       $ displayText txt <> "\n" 
          <> "  BEF " 
            <> displayString (show (fmap displayFact (flattenNnf . nnfFromTerm . fromTerm $ sentence))) <> "\n" 
          <> "  AFT " <> displayString (show (fmap displayFact nnf)) <> "\n"
          <> foldMap (\a -> 
            "  " <> case a of 
                 DDeps x y -> "DEP " <> displayFact x <> " ~~> " <> displayFact y
                 DLit  (Literal b x)  -> "LIT " <> bool "! " "" b <> displayFact x
                 DFalse  -> "FLS "
            <> "\n") 
            edges
       

    return (key, isCore, [ (a, b) | DDeps a b <- edges ])


data LogicConfig = LogicConfig
  { keepHierarchy :: Bool
  }

logic :: LogicConfig -> Hierarchy -> Item -> (Fact, Term Fact)
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
        hasSuperClass (cls^.className) "java/lang/Enum"
        /\ ( requireMethod hry cls . mkAbsMethodId cls 
           $ "values" 
           <:> MethodDescriptor [] 
            (ReturnDescriptor . Just . JTRef . JTArray .JTRef . JTClass $ cls^.className)
           )
        /\ ( requireMethod hry cls . mkAbsMethodId cls 
           $ "valueOf" 
          <:> MethodDescriptor ["Ljava/lang/String;"] 
            (ReturnDescriptor . Just . JTRef . JTClass $ cls^.className)
           )
        /\ ( requireField hry cls . mkAbsFieldId cls 
           $ "$VALUES" 
            <:> FieldDescriptor  (JTRef . JTArray .JTRef . JTClass $ cls^.className)
           )


    , -- We also do also not reduce enclosing methods. If a class is enclosed
      -- in another class, require that to exist, and if the class is enclosed
      -- in a method require that to exist.
      forallOf (classEnclosingMethod._Just) cls
      \(cn, mMId) -> c ==> case mMId of
        Just m -> methodExist (mkAbsMethodId cn m) /\ isInnerClassOf cls cn
        Nothing -> requireClassName cls cn
    ]

  IField (cls, field) -> FieldExist (mkAbsFieldId cls field)
    `withLogic` \f ->
    [ f ==> requireClassNamesOf cls fieldType field
    , f ==> requireClassNamesOf cls (fieldAnnotations.folded) field
    , -- If a field is final it has to be set. This means we cannot stub
      -- class initializers, .
      given (FFinal `S.member` flags ) $
        if FStatic `S.member` flags
        then
          forallOf classInitializers cls \m ->
            f ==> codeIsUntuched m
        else
          forallOf classConstructors cls \m -> 
            f ==> codeIsUntuched m


    , -- TODO: Reconsider this?
      -- If any field is synthetic we will require it to not be removed, if the
      -- class exist. This helps with many problems.
      given (FSynthetic `S.member` flags) do
        classExist cls ==> f
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
          ==> requireMethod hry cls (mkAbsMethodId def method)
      else
      -- If the methods is not abstract, make sure that the method defintion
      -- does exist. A chain from A <: I <: !I. If I does not exit, either
      -- this method have to stay or we have to remove the implements interface.
      forall (superDeclarationPaths (mkAbsMethodId cls method) hry)
        \(decl, isAbstract, path) -> given isAbstract
          $ methodExist decl /\ unbrokenPath path ==> m

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
      -- TODO: Determine access rights
      let mid = mkAbsMethodId (ct^.simpleType) ("<init>:()V" :: MethodId)
      in 
      s ==> 
        case Jvmhs.methodExist mid hry of 
          Just (view stubMethodAccess -> access) 
            | access == Public ->
              (if isJust (Jvmhs.methodExist mid hry) then methodExist mid else false) 
              \/ existOf classConstructors cls codeIsUntuched
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
      c ==> requireClassNamesOf cls (codeExceptionTable.folded) code
    , c ==> requireClassNamesOf cls (codeStackMap._Just) code
    , c ==> requireClassNamesOf cls (codeByteCode.folded) code
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
          /\ (c ==> and [ s `requireSubtype` t | (s, t) <- zip (state ^. tcStack) (reverse stackTypes)])
          where
            (methodRequirements, stackTypes) =
              case methodInvokeTypes a of
                Right (isSpecial, isStatic, m) ->
                  ( let mid = AbsMethodId $ m^.asInClass
                    in (c ==> if isSpecial then methodExist mid else requireMethod hry cls mid)
                        /\ given 
                        (or 
                          [ -- If it is an access$ method it depends on the existance of
                            -- this code to exit
                            Text.isPrefixOf "access$" (m^.methodIdName)
                          , -- If the class is an annoumus class it depends on the existence
                            -- of the code that defines it, execept if it
                            -- call itself.
                            ( Text.all isNumber . last . Text.splitOn "$" 
                            $ mid^.className.fullyQualifiedName
                            ) /\ mid^.className /= cls ^.className
                          ]
                                 ) 
                                 (methodExist mid ==> c) 
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
      -> Term Fact
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

unbrokenPath :: SubclassPath -> Term Fact
unbrokenPath path =
  and [ isSubclass f t e | (f, t, e) <- subclassEdges path]

isSubclass :: ClassName -> ClassName -> HEdge -> Term Fact
isSubclass cn1 cn2 = \case
  Implement -> hasInterface cn1 cn2
  Extend -> hasSuperClass cn1 cn2

hasInterface :: ClassName -> ClassName -> Term Fact
hasInterface cn1 cn2 = tt (HasInterface cn1 cn2)

hasSuperClass :: ClassName -> ClassName -> Term Fact
hasSuperClass cn1 cn2 = tt (HasSuperClass cn1 cn2)

requireMethodHandle :: HasClassName c => Hierarchy -> c -> B.MethodHandle B.High -> Term Fact
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

requireBootstrapMethod :: HasClassName c => c -> Int -> Term Fact
requireBootstrapMethod c i = tt (HasBootstrapMethod (c^.className) i)

requireClassNames :: (HasClassName c, HasClassNames a) => c -> a -> Term Fact
requireClassNames c =
  andOf (classNames . to (requireClassName c))

requireClassName :: (HasClassName c, HasClassName a) => c -> a -> Term Fact
requireClassName oc ic =
  classExist ic /\ isInnerClassOf oc ic


classExist :: HasClassName a =>  a -> Term Fact
classExist (view className -> cn) =
  tt (ClassExist cn)

fieldExist :: AbsFieldId -> Term Fact
fieldExist f =
  tt (FieldExist f)

methodExist :: AbsMethodId -> Term Fact
methodExist f =
  tt (MethodExist f)

requireField :: HasClassName c => Hierarchy -> c -> AbsFieldId -> Term Fact
requireField hry cn fid = isInnerClassOf cn fid /\ or
  [ fieldExist fid' /\ unbrokenPath path
  | (fid', path) <- fieldLocationPaths fid hry
  ]

requireMethod :: HasClassName c => Hierarchy -> c -> AbsMethodId -> Term Fact
requireMethod hry cn mid = isInnerClassOf cn mid /\ or
  [ methodExist mid' /\ unbrokenPath path
  | (mid', _, path) <- superDeclarationPaths mid hry
  ]

codeIsUntuched :: AbsMethodId -> Term Fact
codeIsUntuched m =
  tt (CodeIsUntuched m)

isInnerClassOf :: (HasClassName c1, HasClassName c2) => c1 -> c2 -> Term Fact
isInnerClassOf (view className -> c1) (view className -> c2) =
   given (isInnerClass c2) (tt (IsInnerClass c1 c2))

withLogic :: Fact -> (Term Fact -> [Term Fact]) -> (Fact, Term Fact)
withLogic f fn = (f, and (fn (tt f)))

whenM :: Monad m => (m Bool) -> m () -> m ()
whenM mb m = mb >>= \b -> when b m
