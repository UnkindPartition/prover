{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Ast
  ( Decl(..)
  , Term(..)
  )
  where

import Bound
import Control.Applicative (Applicative(..))
import Control.Monad
import Data.Foldable (Foldable, toList)
import Data.Traversable
import Text.PrettyPrint.Mainland as PP
import Prelude.Extras (Eq1(..))
import Data.Hashable
import Data.Hashable.Extras
import qualified Data.HashSet as HS
import GHC.Generics

type VName = String

----------------------------------------------------------------------
--                           Declarations
----------------------------------------------------------------------

data Decl = Decl VName (forall n . Term n)

instance Show Ast.Decl where
  showsPrec p (Decl n d)
    = showParen (p >= 11) $
      showString "Decl " .
      showsPrec 11 n .
      showString " " .
      showsPrec 11 (d :: Term String)

----------------------------------------------------------------------
--                           Expressions
----------------------------------------------------------------------

data Term n
  = Var   n
  | App   (Term n) (Term n)
  | Lam   (Scope () Term n)
  deriving (Foldable, Traversable, Eq, Generic)

instance Eq1 Term
instance Hashable n => Hashable (Term n)
instance Hashable1 Term

instance Show (Term String) where
  show = show . ppr

instance Functor Term where
  fmap = liftM

instance Applicative Term where
  pure = return
  (<*>) = ap

instance Monad Term where
  return = Var
  e >>= f =
    case e of
      Var n -> f n
      App t1 t2 -> App (t1 >>= f) (t2 >>= f)
      Lam b -> Lam (b >>>= f)

instance Pretty (Term VName) where
  pprPrec p t =
    let
      free = HS.fromList $ toList t
      vars =
        filter (not . flip HS.member free)
        [ [i] | i <- ['a'..'z']] ++ [i : show j | j <- [1..], i <- ['a'..'z'] ]
    in pprPrecTermV vars p t

pprPrecTermV :: [VName] -> Int -> Term VName -> Doc
pprPrecTermV vs p = \case
  Var n -> PP.text n
  App t1 t2 -> parensIf (p >= 2) $
    pprPrecTermV vs 1 t1 <+> pprPrecTermV vs 2 t2
  term@Lam{} -> parensIf (p >= 1) $ pprLambda vs [] term

pprLambda :: [VName] -> [VName] -> Term VName -> Doc
pprLambda vs lambdavars = \case
  Lam b ->
    let v1:vs' = vs
    in pprLambda vs' (v1:lambdavars) (instantiate1 (Var v1) b)
  term ->
    text "λ" <> spread (map text lambdavars) <> dot <+>
    pprPrecTermV vs 0 term
