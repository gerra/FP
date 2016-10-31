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