{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
import FS
import Control.Monad hiding ((>=>))
import Data.Function

instance MonadFish m => Monad m where
    return  = returnFish
    m >>= f =  id >=> (\a -> f a) $ m  
