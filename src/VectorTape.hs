{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module VectorTape where

import           Data.Vector (Vector, (!))
import qualified Data.Vector as V
import           Control.Comonad

-- Stream ----------------------------------------------------------------------

data Stream a = Cons a (Stream a)

instance Functor Stream where
  fmap f ~(Cons x xs) = Cons (f x) (fmap f xs)

repeatS :: a -> Stream a
repeatS x = Cons x (repeatS x)

zipWithS :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWithS f ~(Cons x xs) ~(Cons y ys) = Cons (f x y) (zipWithS f xs ys)

unfoldS :: (c -> (a, c)) -> c -> Stream a
unfoldS f c = let (x, d) = f c in Cons x (unfoldS f d)

instance Applicative Stream where
  pure  = repeatS
  (<*>) = zipWithS ($)

-- Chunk -----------------------------------------------------------------------

newtype Chunk a = Chunk { asVector :: (Vector a) } deriving (Functor)

blockSize :: Int
blockSize = 10

instance Applicative Chunk where
  pure = Chunk . V.replicate blockSize
  (Chunk fs) <*> (Chunk xs) = Chunk (V.zipWith ($) fs xs)

-- Tape ------------------------------------------------------------------------

data Tape a = Tape { chunk :: Chunk a
                   , index :: Int
                   , viewS :: Stream (Chunk a)
                   , viewR :: Stream (Chunk a)
                   }

focus :: Tape a -> a
focus (Tape (Chunk v) i _ _) = v ! i

moveL :: Tape a -> Tape a
moveL (Tape c i ls rs) | i > 0 =
       Tape c (i - 1) ls rs
moveL (Tape c i (Cons c'@(Chunk v') ls) rs) | otherwise =
       Tape c' (V.length v' - 1) ls (Cons c rs)

moveR :: Tape a -> Tape a
moveR (Tape c@(Chunk v) i ls rs) | i < V.length v - 1 =
       Tape c (i + 1) ls rs
moveR (Tape c i ls (Cons c'@(Chunk v') rs)) | otherwise =
       Tape c' (V.length v' - 1) (Cons c ls) rs

instance Functor Tape where
  fmap f (Tape c i ls rs) = Tape (fmap f c) i (fmap (fmap f) ls) (fmap (fmap f) rs)

unfoldT :: (c -> (a, c)) -> (c -> a) -> (c -> (a, c)) -> c -> Tape a
unfoldT prev center next = error "Not implemented"
  --Tape <$> chunk . center <*> index . center <*> unfoldS prev <*> unfoldS next

iterateT :: (a -> a) -> (a -> a) -> a -> Tape a
iterateT prev next = unfoldT (dup . prev) id (dup . next)
  where dup x = (x, x)

instance Comonad Tape where
  extract = focus
  duplicate = iterateT moveL moveR

instance ComonadApply Tape where
  (Tape c i ls rs) <@> (Tape c' i' ls' rs') | i == i' =
    Tape (c <*> c') i ((fmap (<*>) ls) <*> ls') ((fmap (<*>) rs) <*> rs')
  _ <@> _ | otherwise = error "Align your tapes and try again."

instance Applicative Tape where
  (<*>) = (<@>)
  pure  = Tape <$> pure <*> const 0 <*> pure . pure <*> pure . pure
