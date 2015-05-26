module Reduce
  ( reduce
  , nf
  , Lookup
  , lookupFromDecls
  , Reducing(..)
  , Reduction(..)
  , Rule(..)
  , reductionTraverseN
  ) where

import Control.Applicative
import Control.Monad.Logic
import Data.Traversable
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import Text.PrettyPrint.Mainland as PP hiding (empty)
import Bound
import Ast

----------------------------------------------------------------------
--                          'Reducing' class
----------------------------------------------------------------------

-- | Class for performing reductions
class Reducing f where
  -- | Apply a reduction
  rapply :: (term -> Maybe (Reduction n term)) -> f n term -> f n term

  -- | 'fmap' for the second type parameter
  rfmap :: (a -> b) -> f n a -> f n b
  rfmap f a = rpure f `rap` a

  -- | 'pure' for the second type parameter
  rpure :: a -> f n a

  -- | '<*>' for the second type parameter
  rap :: f n (a -> b) -> f n a -> f n b

  -- | 'mapMaybe' for the first type parameter
  rmapMaybe :: (n -> Maybe n') -> f n a -> f n' a

newtype Reduce n a = Reduce a
instance Reducing Reduce where
  rapply fn (Reduce x) =
    Reduce $ maybe x reducedTo (fn x)
  rfmap f (Reduce x) = Reduce $ f x
  rpure = Reduce
  rap (Reduce f) (Reduce x) = Reduce $ f x
  rmapMaybe _ (Reduce x) = Reduce x

----------------------------------------------------------------------
--                          'Reduction' type
----------------------------------------------------------------------

data Rule n
  = Beta
  | Eta
  | Inline n
  deriving (Show, Functor, Foldable, Traversable)

data Reduction n term = Reduction
  { reducedBy   :: Rule n
  , reducedTo   :: term
  }
  deriving (Show, Functor)

reductionTraverseN
  :: Applicative t
  => (n -> t n')
  -> Reduction n  term
  -> t (Reduction n' term)
reductionTraverseN f r = (\by -> r { reducedBy = by }) <$> traverse f (reducedBy r)

instance Pretty (Rule VName) where
  ppr = \case
    Beta -> text "β-reduce"
    Eta -> text "η-reduce"
    Inline n -> text "inline" <+> text n

instance (Pretty (Rule n), Pretty term) => Pretty (Reduction n term) where
  ppr Reduction{..} =
    text "  {-" <+> ppr reducedBy <+> text "-}" </>
    equals <+> ppr reducedTo
  pprList = stack . map ppr

----------------------------------------------------------------------
--                            Reductions
----------------------------------------------------------------------

beta :: Term n -> Maybe (Reduction n (Term n))
beta = fmap (Reduction Beta) . \case
  App (Lam body) arg -> pure $ instantiate1 arg body
  _ -> empty

eta :: Term n -> Maybe (Reduction n (Term n))
eta = fmap (Reduction Eta) . \case
  Lam (fromScope -> App arg (Var (B ())))

    | F term <- sequenceA arg -> pure term

  _ -> empty

inline :: Lookup n -> Term n -> Maybe (Reduction n (Term n))
inline lkp = \case
  Var n | Just d <- lkp n ->
    pure $ Reduction (Inline n) d
  _ -> empty

reduce
  :: forall n0 f . Reducing f
  => Lookup n0
  -> Term n0
  -> f n0 (Term n0)
reduce lkp0 = go lkp0 where
  go :: forall n . Lookup n -> Term n -> f n (Term n)
  go lkp = rapply (inline lkp) . rapply beta . rapply eta . \case
    Var n -> rpure $ Var n
    App t1 t2 -> App `rfmap` go lkp t1 `rap` go lkp t2
    Lam body -> rmapMaybe shrink $
      Lam `rfmap` (rfmap toScope . go (generalize lkp) . fromScope) body

  shrink :: Var x n -> Maybe n
  shrink = \case
    B {} -> Nothing
    F n -> Just n

  generalize :: forall x n . Lookup n -> Lookup (Var x n)
  generalize lkp = lkp <=< shrink

nf :: Eq n => Lookup n -> Int -> Term n -> Maybe (Term n)
nf lkp = go
  where
    go fuel term
      | fuel <= 0 = Nothing
      | otherwise =
        let
          Reduce term' = reduce lkp term
        in
          if term == term'
            then Just term
            else go (fuel-1) term'

----------------------------------------------------------------------
--                           Lookup
----------------------------------------------------------------------

type Lookup n = forall m . n -> Maybe (Term m)

lookupFromDecls :: [Decl] -> Lookup String
lookupFromDecls decls =
  let
    hm = HM.fromList [(name, def) | Decl name def <- decls ]
  in
    flip HM.lookup hm
