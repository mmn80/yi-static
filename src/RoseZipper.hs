{-# LANGUAGE DeriveFunctor #-}

module RoseZipper where

import Prelude hiding (unzip)
import Control.Comonad

data Tree a = Node a [Tree a] deriving Functor
-- use Seq instead of [] to make the `++` and `splitAt` below faster
-- we would also have a fast `lastChild` and other movement operators

data Tree' a = Node' a [Tree a] [Tree a] deriving Functor

(<+) :: Tree' a -> Tree a -> Tree a
(Node' x ls rs) <+ t = Node x (ls ++ [t] ++ rs)

data Layout a = Layout { focus   :: Tree a
                       , context :: [Tree' a]
                       } deriving Functor

unzip :: Layout a -> Tree a
unzip (Layout t (c : cs)) = unzip (Layout (c <+ t) cs)
unzip (Layout t []) = t

toParent :: Layout a -> Layout a
toParent (Layout _ []) = error "Adam's parent is God's bottom!"
toParent (Layout t (c : cs)) = Layout (c <+ t) cs

toChildNo :: Int -> Layout a -> Layout a
toChildNo idx (Layout (Node x ts) cs) = Layout t (Node' x ls rs : cs)
  where (ls, t : rs) = splitAt idx ts

instance Comonad Layout where
  extract (Layout (Node x _) _) = x
  duplicate l@(Layout (Node x ts) cs) =
    Layout (Node l (_1 <$> ts)) (_2 <$> cs)
-- _1: do not use `toChildNo`, but fold manually and use the implicit context