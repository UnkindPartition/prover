{-# LANGUAGE LambdaCase, ViewPatterns, ScopedTypeVariables, RankNTypes #-}
module Reduce where

import Control.Monad.Logic
import Control.Applicative
import Data.Generics.Geniplate
import Data.Traversable
import qualified Data.HashMap.Strict as HM
import GHC.Generics
import Bound
import Ast
import Wiggle

beta :: Alternative t => Term n -> t (Term n)
beta = \case
  App (Lam body) arg -> pure $ instantiate1 arg body
  _ -> empty

eta :: Alternative t => Term n -> t (Term n)
eta = \case
  Lam (fromScope -> App arg (Var (B ())))

    | F term <- sequenceA arg -> pure term

  _ -> empty

type Lookup n = forall m . n -> Maybe (Term m)

inline :: Alternative t => Lookup n -> Term n -> t (Term n)
inline lkp = \case
  Var n | Just d <- lkp n -> pure d
  _ -> empty

reduce :: forall n t . Alternative t => Lookup n -> Term n -> t (Term n)
reduce lkp = wiggled . go lkp where
  go :: forall n t . Alternative t => Lookup n -> Term n -> W t (Term n)
  go lkp = wapply (inline lkp) . wapply beta . wapply eta . \case
    Var n -> pure $ Var n
    App t1 t2 -> App <$> go lkp t1 <*> go lkp t2
    Lam body -> Lam <$> (fmap toScope . go (generalize lkp) . fromScope) body

  generalize :: forall x n . Lookup n -> Lookup (Var x n)
  generalize lkp = \case
    B {} -> Nothing
    F n -> lkp n
