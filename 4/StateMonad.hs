data State s v = State { f :: s -> (v, s) }

-- m a -> (a -> m b) -> m b

instance Monad (State s) where
    return v = State $ (\s -> (v, s))
    m >>= g  = State $ ((\s -> f (g $ fst $ f m s) s))

    --m >>= g  = State $ ((\s -> f (g $ fst $ f m s) s))
    --(State f) >>= g  = State $ (\s -> next (g (fst (f s))) s)

newtype Book = Book Int

data DataBook = DataBook Int