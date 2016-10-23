{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
import FS

import Control.Monad
import Data.Function

instance Monad m => MonadFish m where
    returnFish = return
    f >=> g    = \x -> f x >>= g