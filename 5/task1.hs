import Control.Applicative
import Data.Monoid
import Data.Foldable

newtype MyIdentity a = MyIdentity { runIdentity :: a }
	deriving Show

data MyEither a b = MyLeft a | MyRight b

type Forest a = [Tree a]
data Tree a = Node {
        rootLabel :: a,         -- ^ label value
        subForest :: Forest a   -- ^ zero or more child trees
    }

newtype MyConst a b = MyConst { getConst :: a }

data MyPair a b = MyPair a b
