{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Control.Lens hiding (andOf, orOf)

-- containers
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.IntSet as IS

-- vector
import qualified Data.Vector as V

-- base
import Data.Foldable hiding (and, or)
import Data.Maybe
import Data.Monoid
import Control.Monad.Fail
import Prelude hiding (fail, not, and, or)

-- jvmhs
import Jvmhs.Data.Type
import Jvmhs.TypeCheck
import Jvmhs.Data.Signature
import Jvmhs.Data.Code
import Jvmhs

-- jvm-binary
import qualified Language.JVM.Attribute.BootstrapMethods as B
import qualified Language.JVM as B
import Language.JVM.ByteCode (ByteCodeOpr (..))

import Control.Reduce.Boolean
import Control.Reduce.Problem

-- jreduce
import JReduce.Target
import JReduce.Config
import JReduce.OverDeep ( Item (..), Fact (..)
                        , classInitializers, classConstructors
                        , itemR, displayFact
                        , _ITarget
                        )

data GraphLang a = GVar a | Deps a a

underCompiler :: Term Int -> Maybe [ GraphLang Int ]
underCompiler = fmap catMaybes . mapM fn . cnfCompiler where
  fn (Clause trs fls) = case (IS.toList trs, IS.toList fls) of
      ([] , [] )   -> fail "False"
      ([] , [a])  -> return $ Just (GVar a)
      ([_], [] )  -> fail "Illigal expression"
      ([a], [b]) -> return $ Just (Deps b a)
      _ -> return $ Nothing

overCompiler :: Term Int -> Maybe [ GraphLang Int ]
overCompiler = mapM fn . cnfCompiler where
  fn (Clause trs fls) = case (IS.toList trs, IS.toList fls) of
      ([] , [] ) -> fail "False"
      ([] , a:_) -> return $ GVar a
      (_:_, [] )  -> fail "Illigal expression"
      (a:_, b:_) -> return $ Deps b a

decompose :: Ord a => S.Set a -> (M.Map a Int, V.Vector a)
decompose a =
  (M.fromAscList . toList $ imap (flip (,)) res, res)
  where res = V.fromList (toList a)

describeGraphProblem ::
  MonadIOReader Config m
  => Bool
  -> FilePath
  -> Problem a Target
  -> m (Problem a [IS.IntSet])
describeGraphProblem b wf p = do
  describeProblemTemplate itemR genKeyFun displayFact _ITarget wf p
  where
    genKeyFun = do
      let targets = targetClasses $ _problemInitial p
      let scope = S.fromList ( map (view className) targets)
      hry <- fetchHierachy targets
      pure $ \i ->
        let (f, tf) = logic scope hry i
            (it, res) = decompose (foldMap S.singleton tf)
            tf2 = fmap (it M.!) tf
        in
          ( f
          , [ (res V.! x, res V.! y)
            | Deps x y <- concat
              $ if b
                then overCompiler tf2
                else underCompiler tf2
            ]
          )


logic :: S.Set ClassName -> Hierarchy -> Item -> (Fact, Term Fact)
logic scope hry = \case

  IContent (ClassFile cls) -> ClassExist (cls ^. className)
    `withLogic` \c ->
    [ -- We do currently not reduce bootstrap methods, so their requirements are
      -- handled from here.
      forallOf (classBootstrapMethods.folded._Wrapped) cls
      \(B.BootstrapMethod mhandle args) ->
        c ==> requireMethodHandle mhandle /\ requireClassNamesOf folded args

    , -- We also do not reduce type parameters. Thier requirements are just that
      -- all classes mention should exist if this class exist.
      c ==> requireClassNamesOf (classTypeParameters.folded) cls

    , -- We also do also not reduce enclosing methods. If a class is enclosed
      -- in another class, require that to exist, and if the class is enclosed
      -- in a method require that to exist.
      forallOf (classEnclosingMethod._Just) cls
      \(cn, mMId) -> c ==> case mMId of
        Just m -> methodExist (mkAbsMethodId cn m)
        Nothing -> classExist cn
    ]

  IField (cls, field) -> FieldExist (mkAbsFieldId cls field)
    `withLogic` \f ->
    [ -- If a field is final it has to be set. This happens either in the
      -- class initializers or in the constructors. This means we cannot stub
      -- these methods.
      given (FFinal `S.member` flags)
        if FStatic `S.member` flags
        then forallOf classInitializers cls \m ->
          f ==> codeIsUntuched m
        else forallOf classConstructors cls \m ->
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
      m ==> requireClassNamesOf
      (methodReturn ._Just <> methodArguments.folded)
      method

    , -- Type parameters might contain classes
      m ==> requireClassNamesOf
      (methodTypeParameters.folded)
      method

    , if method^.methodAccessFlags.contains MAbstract
      then
      -- If a method is abstract, then it has to be implemented in all of its
      -- implementations. Say that A implements I and is not abstract, then
      -- it should implement all the methods of I. For each such path, we
      -- require it to be true or one of it's super classes to have implemented
      -- it.
      forall (implementationPaths (cls^.className) hry)
        \(def, isAbstract, path) -> given (not isAbstract)
          $ m /\ unbrokenPath path ==> requireMethod (mkAbsMethodId def method)
      else
      -- If the methods is not abstract, make sure that the method defintion
      -- does exist. A chain from A <: I <: !I. If I does not exit, either
      -- this method have to stay or we have to remove the implements interface.
      forall (superDeclarationPaths (mkAbsMethodId cls method) hry)
        \(decl, isAbstract, path) -> given
          (isAbstract && not ((decl ^. className) `S.member` scope)) $
          unbrokenPath path ==> m

    , -- TODO: Nessary?
      -- Finally we requier that if a method is synthetic is should be
      -- removed alongside its code
      given (method^.methodAccessFlags.contains MSynthetic)
      $ m ==> codeIsUntuched (mkAbsMethodId cls method)
    ]

  IImplements (cls, ct) -> HasInterface (cls^.className) (ct^.classTypeName)
    `withLogic` \i ->
    [ -- An Implements only depends on the interface that it implements, and
      -- its type parameters.
      i ==> requireClassNames ct
    ]

  ISuperClass (cls, ct) -> HasSuperClass (cls^.className) (ct^.classTypeName)
    `withLogic` \s ->
    [ -- An Implements only depends on the class of the supertype and its type
      -- parameters.
      s ==> requireClassNames ct
    ]

  IInnerClass (cls, ic) -> IsInnerClass (cls^.className) (ic^.innerClass)
    `withLogic` \i ->
    [ -- An innerclass depends on all classes referenced by the innerClass.
      i ==> requireClassNames ic

    , -- If inner class is ponting to itself, then it required as long at the
      -- class exist.
      given (cls^.className == ic^.innerClass) $
      classExist cls ==> i

      -- NOTE: That all requirements that a class exist also will check if
      -- the innerclass exist. The rule is that if a class refer to an innerclass
      -- it must have an innerclass entry that describes that class.
    ]

  IMethodThrows ((cls, method), mt) ->
    MethodThrows (mkAbsMethodId cls method)
    (fromMaybe "java/lang/Throwable" (mt^?_ThrowsClass.classTypeName))
    `withLogic` \m ->
    [ -- A method throws statement depends on all the class it mentions.
      m ==> requireClassNames mt

      -- TODO: An indepth analysis of throws of the code?
    , codeIsUntuched (mkAbsMethodId cls method) ==> m
    ]

  ICode ((cls, method), code) -> CodeIsUntuched theMethodName
    `withLogic` \c ->
    [ -- If the code was not stubbed, then we have to require that the
      -- classes in the exception table, stack map, and byte-code instructions
      -- exits
      c ==> requireClassNamesOf (codeExceptionTable.folded) code
    , c ==> requireClassNamesOf (codeStackMap._Just) code
    , c ==> requireClassNamesOf (codeByteCode.folded) code
    ] ++
    [ c ==> case oper of
        ArrayStore _ ->
          -- When we store an item in the array, it should be a subtype of the
          -- content of the array.
          stack 0 `requireSubtype` isArray (stack 2)

        Get fa fid ->
          -- For a get value is valid the field has to exist, and the first
          -- element on the stack has to be a subclass of fields class.
          requireField fid
          /\ given (fa /= B.FldStatic) (stack 0 `requireSubtype` fid^.className)

        Put fa fid ->
          -- For a put value is valid the field has to exist, and the first
          -- element on the stack has to be a subclass of fields class, and
          -- the second element have to be a subtype of the type of the field
          requireField fid
          /\ given (fa /= B.FldStatic) (stack 0 `requireSubtype` fid^.className)
          /\ stack 1 `requireSubtype` fid^.fieldType

        Invoke a ->
          -- For the methods there are three general cases, a regular method call,
          -- a static methods call (no-object) and a dynamic call (no-class).
          methodRequirements
          /\ and [ s `requireSubtype` t | (s, t) <- zip (state ^. tcStack) (reverse stackTypes)]
          where
            (methodRequirements, stackTypes) =
              case methodInvokeTypes a of
                Right (isStatic, m) ->
                  ( requireMethod (AbsMethodId $ m^.asInClass)
                  , [asTypeInfo $ m^.asInClass.className | not isStatic]
                    <> (map asTypeInfo $ m^.methodArgumentTypes)
                  )
                Left m -> (true, map asTypeInfo $ m^.methodArgumentTypes)

        Throw ->
          -- A Throw operation requires that the first element on the stack is throwable.
          stack 0 `requireSubtype` ("java/lang/Throwable" :: ClassName)

        CheckCast fa ->
          -- The check cast operation requires that the first element on the stack
          -- is either a subtype of the cast or the cast is a subtype of the first
          -- element. Often only one of these are true.
          stack 0 `requireSubtype` fa \/ fa `requireSubtype` stack 0

        Return (Just B.LRef) ->
          -- We do require that the first element on the stack is a subtype of the return type.
          forall (method^.methodReturnType)
          \mt -> stack 0 `requireSubtype` mt

        _ -> true
    | (state, B.opcode -> oper) <-
        V.toList $ V.zip typeCheckStates (code ^. codeByteCode)
    , let stack n = state ^?! tcStack.ix n
    ]
    where
      theMethodName =
        mkAbsMethodId cls method

      methodInvokeTypes = \case
        B.InvkSpecial (B.AbsVariableMethodId _ m) -> Right (False, m)
        B.InvkVirtual m -> Right (False, m)
        B.InvkStatic (B.AbsVariableMethodId _ m) -> Right (True, m)
        B.InvkInterface _ (B.AbsInterfaceMethodId m) -> Right (False, m)
        B.InvkDynamic (B.InvokeDynamic _ m') -> Left m'

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

    requireField fid = or
      [ fieldExist fid' /\ unbrokenPath path
      | (fid', path) <- fieldLocationPaths fid hry
      ]

    requireMethod mid = or
      [ methodExist mid' /\ unbrokenPath path
      | (mid', _, path) <- superDeclarationPaths mid hry
      ]

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
hasInterface cn1 cn2 = TVar (HasInterface cn1 cn2)

hasSuperClass :: ClassName -> ClassName -> Term Fact
hasSuperClass cn1 cn2 = TVar (HasSuperClass cn1 cn2)

requireMethodHandle :: B.MethodHandle B.High -> Term Fact
requireMethodHandle = undefined

requireClassNames :: Inspectable a => a -> Term Fact
requireClassNames = andOf (classNames . to classExist)

requireClassNamesOf ::
  Inspectable a
  => Getting (Endo (Endo (Term Fact))) s a -> s -> (Term Fact)
requireClassNamesOf l a =
  forallOf (l . classNames) a classExist

classExist :: HasClassName a => a -> Term Fact
classExist cn = TVar (ClassExist (cn^.className))

fieldExist :: AbsFieldId -> Term Fact
fieldExist f =
  TVar (FieldExist f)

methodExist :: AbsMethodId -> Term Fact
methodExist f =
  TVar (MethodExist f)

codeIsUntuched :: AbsMethodId -> Term Fact
codeIsUntuched m =
  TVar (CodeIsUntuched m)


withLogic :: Fact -> (Term Fact -> [Term Fact]) -> (Fact, Term Fact)
withLogic f fn = (f, and (fn (TVar f)))

