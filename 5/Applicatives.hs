module Applicatives where

import MyDatas
import Data.Monoid
import Control.Applicative
import Functors

instance Applicative MyIdentity where
    pure = MyIdentity
    (MyIdentity f) <*> (MyIdentity i) = MyIdentity (f i)

instance Applicative (MyEither a) where
    pure = MyRight
    MyLeft  f <*> _ = MyLeft f
    MyRight f <*> a = fmap f a 

instance Applicative Tree where
    pure a = Node a []
    Node f cf <*> t@(Node x cx) = 
        Node (f x) (map (fmap f) cx ++ map (<*> t) cf)

instance Monoid a => Applicative (MyConst a) where
    pure a = MyConst mempty
    (MyConst f) <*> (MyConst c) = MyConst (mappend f c)

instance Monoid a => Applicative (MyPair a) where
    pure a = MyPair mempty a
    (MyPair a1 f) <*> (MyPair a2 b2) = MyPair (mappend a1 a2) (f b2)