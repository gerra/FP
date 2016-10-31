import MyDatas
import Data.Traversable
import Control.Applicative
import Foldables
import Functors
import Applicatives

instance Traversable MyIdentity where
    traverse f (MyIdentity i) = MyIdentity <$> (f i)

instance Traversable (MyEither a) where
    traverse _ (MyLeft  x) = pure (MyLeft x)
    traverse f (MyRight x) = MyRight <$> f x

instance Traversable Tree where
    traverse f (Node a ts) = Node <$> (f a) <*> (traverse (traverse f) ts)

instance Traversable (MyConst a) where
    traverse _ (MyConst m) = pure (MyConst m)

instance Traversable (MyPair a) where
    traverse f (MyPair x y) = (MyPair x) <$> f y 