-- adapted from:
-- https://www.reddit.com/r/haskell/comments/11mnw5/wiggling_sums_a_somewhat_interesting_traversable/c6nzowc

module Wiggle (W(..), wiggled, wapply) where

import Control.Applicative

data W t a = W (t a) a
instance Functor t => Functor (W t) where
    fmap f (W xs x) = W (fmap f xs) (f x)
instance Alternative t => Applicative (W t) where
    pure a = W empty a
    W fs f <*> W xs x = W (fmap ($ x) fs <|> fmap f xs) (f x)

wiggled :: W t a -> t a
wiggled (W xs _) = xs

wapply :: Alternative t => (a -> t a) -> W t a -> W t a
wapply f (W xs x) = W (xs <|> f x) x
