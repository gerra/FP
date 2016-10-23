{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
import FS
import Control.Monad
import Data.Function

instance Monad m => MonadJoin m where
    returnJoin = return
    join x     = x >>= id