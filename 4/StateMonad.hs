data State s v = State { f :: s -> (v, s) }

-- m a -> (a -> m b) -> m b

instance Monad (State s) where
    return v = State $ (\s -> (v, s))
    m >>= g  = State $ (\s -> f (g $ getValue $ f m s) s)
        where getValue (a, b) = a