module Foldables where

import Data.Foldable
import Data.Monoid
import MyDatas

instance Foldable MyIdentity where
    foldMap f (MyIdentity a) = f a

instance Foldable (MyEither a) where
    foldMap _ (MyLeft _)  = mempty
    foldMap f (MyRight a) = f a

instance Foldable Tree where
    foldMap f (Node a ts) 
        = mappend (f a) (foldMap (foldMap f) ts)

instance Foldable (MyConst a) where
    foldMap _ _ = mempty

instance Foldable (MyPair a) where
    foldMap f (MyPair a b) = f b

length :: f a -> Int
--(a -> b -> a) -> f b
-- (a -> b) -> f a -> f b
length x = foldMap (+1) foldl ((+)) x 0

traverse :: Applicative f => (a -> f b) -> (m a) -> (f (m b))

(Either Double) trav

x :: Either Double Int
traverse show x ::  (Either Double) Char