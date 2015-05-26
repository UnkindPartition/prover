module Equality where

import Prelude hiding (sum)
import Control.Monad.Logic
import Control.Applicative
import Data.Foldable
import Data.Functor.Compose
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Data.Tuple.Homogenous
import Data.Ord (comparing)
import Ast
import Reduce
import Wiggle

newtype Gather n term = Gather (W (Compose Logic (Reduction n)) term)

instance Reducing Gather where
  rapply fn (Gather (W xs x)) = Gather $ W (xs `fplus` Compose (asum . fmap pure $ fn x)) x
  rpure = Gather . pure
  rap (Gather f) (Gather x) = Gather (f <*> x)
  rmapMaybe (f :: n -> Maybe n') (Gather (W xs x)) = Gather (W (f' xs) x)
    where

    f' 
      :: Compose Logic (Reduction n ) a 
      -> Compose Logic (Reduction n') a 
    f' (Compose m) = Compose $ asum . fmap pure . reductionTraverseN f =<< m

gathered :: Gather n term -> [Reduction n term]
gathered (Gather (W (Compose xs) _)) = observeAll xs

type RMap n = HM.HashMap (Term n) [Reduction n (Term n)]

equal
  :: forall n . (Hashable n, Eq n)
  => Lookup n
  -> Int -- ^ fuel
  -> Tuple2 (Term n)
  -> Maybe (Tuple2 [Reduction n (Term n)])
equal lkp fuel0 terms =
  let
    termsHS = flip HM.singleton [] <$> terms
  in
    go fuel0 termsHS termsHS

  where

  go :: Int -> Tuple2 (RMap n) -> Tuple2 (RMap n) -> Maybe (Tuple2 [Reduction n (Term n)])
  go fuel accd new
    | fuel <= 0 = Nothing

    | Tuple2 (t1, t2) <- accd,
      let common = HM.intersectionWith (,) t1 t2,
      not $ HM.null common
      = Just . fmap reverse .
        maximumBy (comparing $ sum . fmap length) .
        map Tuple2 $ HM.elems common
    
    | otherwise =
      let
        new' = flip fmap new $
          HM.unions .
          map reduce1 .
          HM.toList
      in
        go (fuel-1)

          -- prefer old entries, since they are shorter
          (HM.union <$> accd <*> new')

          -- do not reduce old entries repeatedly
          (HM.difference <$> new' <*> accd)

  reduce1 :: (Term n, [Reduction n (Term n)]) -> RMap n
  reduce1 (t, reductions) =
    HM.fromList .
    map (\r -> (reducedTo r, r:reductions)) .
    gathered . reduce lkp $ t
