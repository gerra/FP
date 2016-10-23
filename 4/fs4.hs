{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
import FS
import Control.Monad hiding ((>=>))
import Data.Function

instance MonadFish m => MonadJoin m where
    returnJoin = returnFish
    join x = id >=> id $ x