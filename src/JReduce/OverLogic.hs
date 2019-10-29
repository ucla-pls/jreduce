{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module JReduce.OverLogic where

-- lens
import Control.Lens

-- containers
import qualified Data.Set as S

-- mtl
import Control.Monad.Writer

-- base
-- import Control.Monad
import Data.List.NonEmpty (NonEmpty (..))

-- jvmhs
import Jvmhs.Data.Type
import Jvmhs

-- jvm-binary
import qualified Language.JVM.Attribute.BootstrapMethods as B
import qualified Language.JVM as B

-- jreduce
import JReduce.Target
import JReduce.OverDeep (Item (..), Fact (..), classInitializers, classConstructors)


-- | NNF
data Term a
  = And [Term a]
  | Or  [Term a]
  | Var Bool a
  deriving (Show, Eq)

instance Semigroup (Term a) where
  (<>) = (/\)

instance Monoid (Term a) where
  mempty = And []


notL :: Term a -> Term a
notL = \case
  And terms -> Or (map notL terms)
  Or terms -> And (map notL terms)
  Var x a -> Var (not x) a

infixr 8 ==>
(==>) :: Term a -> Term a -> Term a
(==>) a b =
  notL a \/ b

infixl 6 /\
(/\) :: Term a -> Term a -> Term a
a /\ b = Or [ a , b ]

infixl 5 \/
(\/) :: Term a -> Term a -> Term a
a \/ b = And [ a , b ]

withLogic :: Fact -> (Term Fact -> Writer (Term Fact) ()) -> (Fact, Term Fact)
withLogic f fn = (f, execWriter (fn (Var True f)))

logic :: Hierarchy -> Item -> (Fact, Term Fact)
logic hry = \case

  IContent (ClassFile cls) -> ClassExist (cls ^. className)
    `withLogic` \c -> do
    -- We do currently not reduce bootstrap methods, so their requirements are
    -- handled from here.
    forMOf_ (classBootstrapMethods.folded._Wrapped) cls $
      \(B.BootstrapMethod mhandle args) -> do
        tell $ c ==> requireMethodHandle mhandle
          /\ requireClassNamesOf folded args

    -- We also do not reduce type parameters. Thier requirements are just that
    -- all classes mention should exist if this class exist.
    tell $ c ==> requireClassNamesOf (classTypeParameters.folded) cls

    -- We also do also not reduce enclosing methods. If a class is enclosed
    -- in another class, require that to exist, and if the class is enclosed
    -- in a method require that to exist.
    forMOf_ (classEnclosingMethod._Just) cls $ \(cn, mMId) ->
      tell $ c ==> case mMId of
        Just m -> methodExist (mkAbsMethodId cn m)
        Nothing -> classExist cn

  IField (cls, field) -> FieldExist (mkAbsFieldId cls field)
    `withLogic` \f -> do
    -- If a field is final it has to be set. This happens either in the
    -- class initializers or in the constructors. This means we cannot stub
    -- these methods.
    let flags = field^.fieldAccessFlags
    when (FFinal `S.member` flags) $ do
      case FStatic `S.member` flags of
        True -> forMOf_ classInitializers cls $ \m ->
          tell $ f ==> codeIsUntuched m
        False -> forMOf_ classConstructors cls $ \m ->
          tell $ f ==> codeIsUntuched m

    -- TODO: Reconsider this?
    -- If any field is synthetic we will require it to not be removed, if the
    -- class exist. This helps with many problems.
    when (FSynthetic `S.member` flags) $ do
      tell $ classExist cls ==> f

  IMethod (cls, method) -> MethodExist (mkAbsMethodId cls method)
    `withLogic` \m -> do
    -- Since we do not remove the return argument or the arguemnts we have to build
    -- their requirements here.
    tell $ m ==> requireClassNamesOf
      (methodReturn ._Just <> methodArguments.folded) method

    -- Type parameters might contain classes
    tell $ m ==> requireClassNamesOf
      (methodTypeParameters.folded)
      method

    if method^.methodAccessFlags.contains MAbstract
      then do
      -- If a method is abstract, then it has to be implemented in all of its
      -- implementations. Say that A implements I and is not abstract, then
      -- it should implement all the methods of I. For each such path, we
      -- require it to be true or one of it's super classes to have implemented
      -- it.
      forM_ (implementationPaths (cls^.className) hry) $
        \(def, path) -> tell $ Or
          [ m /\ And [ isSubclass f t e | (f, t, e) <- toList path]
            ==>
            methodExist trg
            /\ And [ isSubclass f t e | (f, t, e) <- spath]
          | (trg, spath) <- superDefinitionPaths (mkAbsMethodId def method) hry
          ]
      else do
      -- If the methods is not abstract, make sure that the method defintion
      -- does exist. A chain from A <: I <: !I. If I does not exit, either
      -- this method have to stay or we have to remove the implements interface.
      forM_ (superDeclarationPaths (mkAbsMethodId cls method) hry) $
        \(decl, path) -> unless (decl ^. className `S.member` scope) $ do
          tell $ And [ isSubclass f t e | (f, t, e) <- path ] ==> m


requireMethodHandle :: B.MethodHandle B.High -> Term Fact
requireMethodHandle = undefined

requireClassNames :: Inspectable a => a -> Term Fact
requireClassNames a = a ^. classNames . to classExist

requireClassNamesOf :: (Inspectable a) => Getting (Term Fact) b a -> b -> Term Fact
requireClassNamesOf l b = b ^. l . classNames . to classExist

classExist :: HasClassName a => a -> Term Fact
classExist cn = Var True (ClassExist (cn^.className))

fieldExist :: AbsFieldId -> Term Fact
fieldExist f =
  Var True (FieldExist f)

methodExist :: AbsMethodId -> Term Fact
methodExist f =
  Var True (MethodExist f)

codeIsUntuched :: AbsMethodId -> Term Fact
codeIsUntuched m =
  Var True (CodeIsUntuched m)
