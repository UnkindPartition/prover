{-# LANGUAGE LambdaCase, ViewPatterns, ScopedTypeVariables, RankNTypes #-}
module Reduce where

import Control.Monad.Logic
import Control.Applicative
import Data.Generics.Geniplate
import Data.Traversable
import Data.Functor.Identity
import Data.Maybe
import Data.Proxy
import qualified Data.HashMap.Strict as HM
import GHC.Generics
import Bound
import Ast
import Wiggle

-- | Class for performing reductions.
--
-- Has two instances: one ('W') is for reducing one thing at a time, another
-- ('Identity') is for doing as many reductions at once as possible.
class Applicative f => Reducing f where
  rapply :: (forall t . Alternative t => term -> t term) -> f term -> f term

instance Alternative t => Reducing (W t) where
  rapply fn = wapply fn

instance Reducing Identity where
  rapply fn (Identity x) =
    Identity $ fromMaybe x (fn x)

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

reduce
  :: forall n f . Reducing f
  => Lookup n
  -> Term n
  -> f (Term n)
reduce lkp = go lkp where
  go :: forall n . Lookup n -> Term n -> f (Term n)
  go lkp = rapply (inline lkp) . rapply beta . rapply eta . \case
    Var n -> pure $ Var n
    App t1 t2 -> App <$> go lkp t1 <*> go lkp t2
    Lam body -> Lam <$> (fmap toScope . go (generalize lkp) . fromScope) body

  generalize :: forall x n . Lookup n -> Lookup (Var x n)
  generalize lkp = \case
    B {} -> Nothing
    F n -> lkp n

nf :: Eq n => Lookup n -> Int -> Term n -> Maybe (Term n)
nf lkp = go
  where
    go fuel term
      | fuel <= 0 = Nothing
      | otherwise =
        let
          Identity term' = reduce lkp term
        in
          if term == term'
            then Just term
            else go (fuel-1) term'
