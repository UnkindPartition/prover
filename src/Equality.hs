module Equality where

import Control.Monad.Logic
import Control.Applicative
import Data.Traversable
import Data.Foldable
import Data.Functor.Compose
import qualified Data.HashSet as HS
import Data.Hashable
import Data.Tuple.Homogenous
import Ast
import Reduce
import Wiggle

equal
  :: forall n . (Hashable n, Eq n)
  => Lookup n
  -> Int -- ^ fuel
  -> Tuple2 (Term n)
  -> Bool
equal lkp fuel terms =
  let
    termsHS = HS.singleton <$> terms
  in
    go fuel termsHS termsHS

  where

  go :: Int -> Tuple2 (HS.HashSet (Term n)) -> Tuple2 (HS.HashSet (Term n)) -> Bool
  go fuel accd new
    | fuel <= 0 = False

    | Tuple2 (t1, t2) <- accd,
      not . HS.null $ t1 `HS.intersection` t2
      = True
    
    | otherwise =
      let
        new' = flip fmap new $
          HS.fromList .
          observeAll .
          asum .
          fmap (wiggled . reduce lkp) .
          HS.toList
      in go (fuel-1) (HS.union <$> accd <*> new') new'
