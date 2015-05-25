{-# LANGUAGE OverloadedStrings, LambdaCase, PatternSynonyms, FlexibleInstances, 
             DeriveFoldable, DeriveTraversable, ScopedTypeVariables, KindSignatures, DeriveGeneric,
             RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Ast where

import Bound
import Control.Applicative (Applicative(..))
import Control.Monad
import Data.Foldable (Foldable, toList)
import Data.Traversable
import Text.PrettyPrint.Mainland as PP
import Prelude.Extras (Eq1(..), Show1(..))
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

{-
instance Show1 Term where
  showsPrec1 = showsPrec
-}
instance Show (Term String) where
  show t =
    let
      free = HS.fromList $ toList t
      vars =
        filter (not . flip HS.member free)
        [ [i] | i <- ['a'..'z']] ++ [i : show j | j <- [1..], i <- ['a'..'z'] ]
    in show $ pprPrecTerm vars 0 t

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

pprPrecTerm :: [VName] -> Int -> Term VName -> Doc
pprPrecTerm vs p = \case
  Var n -> PP.text n
  App t1 t2 -> parensIf (p >= 2) $
    pprPrecTerm vs 1 t1 <+> pprPrecTerm vs 2 t2
  Lam b -> parensIf (p >= 1) $
    let v1:vs' = vs in
    text ("\\" ++ v1) <> text "." <+>
    pprPrecTerm vs' 0 (instantiate1 (Var v1) b)
