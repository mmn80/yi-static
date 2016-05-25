module LayoutFix where

import Control.Comonad
import Data.Monoid ((<>))
import Data.Function (fix)
import VectorTape
import qualified RoseZipper as Z

data Rect d = Rect { left :: d, top :: d, width :: d, height :: d }

instance (Ord d, Num d) => Monoid (Rect d) where
  mempty = Rect 0 0 0 0
  (Rect x y dx dy) `mappend` (Rect x' y' dx' dy') =
      Rect (max x x') (max y y') (sz x x' dx dx') (sz y y' dy dy') 
    where sz a a' s s' = max 0 $ min (a + s) (a' + s') - max a a'

data Layout a = Node { cell :: a, parent :: Maybe (Layout a) }

instance Functor Layout where
  fmap f (Node x p) = Node (f x) (fmap (fmap f) p)

instance Comonad Layout where
  extract = cell
  duplicate l@(Node _ p) = Node l (fmap duplicate p)

instance ComonadApply Layout where
  (Node f (Just pf)) <@> (Node x (Just px)) = Node (f x) (Just $ pf <@> px)
  (Node f _) <@> (Node x _) = Node (f x) Nothing

loeb :: Functor f => f (f a -> a) -> f a
loeb fs = xs where xs = fmap ($ xs) fs
-- loeb fs = fix $ \xs -> fmap ($ xs) fs
-- kfix :: ComonadApply w => w (w a -> a) -> w a
-- kfix w = fix $ \u -> w <@> duplicate u
-- kfix w = fix $ (w <@>) . duplicate
-- fix f = let x = f x in x

evaluateF :: (ComonadApply w, Functor f) => w (f (w (f a) -> a)) -> w (f a)
evaluateF fs = fix $ (<@> fs) . fmap (fmap . flip ($)) . duplicate
