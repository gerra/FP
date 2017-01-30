module Functors where

import MyDatas

instance Functor MyIdentity where
    fmap f (MyIdentity i) = MyIdentity (f i)

instance Functor (MyEither a) where
    fmap _ (MyLeft x)  = MyLeft x
    fmap f (MyRight x) = MyRight (f x)

instance Functor Tree where
    fmap f (Node a ts) = Node (f a) (map (fmap f) ts)

instance Functor (MyConst a) where
    fmap _ (MyConst c) = (MyConst c)

instance Functor (MyPair a) where
    fmap f (MyPair a b) = MyPair a (f b)

<$ :: a -> f b -> f a
a <$ x = fmap (const a) x

