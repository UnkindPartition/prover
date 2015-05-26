-- adapted from:
-- https://www.reddit.com/r/haskell/comments/11mnw5/wiggling_sums_a_somewhat_interesting_traversable/c6nzowc

module Wiggle (W(..), wiggled) where

import Control.Applicative
import Control.Monad.Logic
import Data.Functor.Compose

----------------------------------------------------------------------
--                           FSum class
----------------------------------------------------------------------

-- | Like Alternative, but with no Applicative superclass
-- constraint.
--
-- This is important, as Compose Logic Reduction is a functor and FSum, but
-- not Applicative (or Alternative).
class Functor f => FSum f where
  fempty :: f a

  default fempty :: Alternative f => f a
  fempty = empty
  
  fplus :: f a -> f a -> f a

  default fplus :: Alternative f => f a -> f a -> f a
  fplus = (<|>)

instance FSum Logic

instance (FSum f, Functor g) => FSum (Compose f g) where
  fempty = Compose fempty
  fplus (Compose a) (Compose b) = Compose (fplus a b)

data W t a = W (t a) a
instance Functor t => Functor (W t) where
    fmap f (W xs x) = W (fmap f xs) (f x)
instance FSum t => Applicative (W t) where
    pure a = W fempty a
    W fs f <*> W xs x = W (fmap ($ x) fs `fplus` fmap f xs) (f x)

wiggled :: W t a -> t a
wiggled (W xs _) = xs
