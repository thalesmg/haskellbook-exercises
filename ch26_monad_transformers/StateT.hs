newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
  fmap f (StateT smas) =
    StateT $ \s -> ff (smas s)
    where
      ff = fmap (\(a, s) -> (f a, s))

instance Applicative m => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)
  (StateT sfab) <*> (StateT smas) =
    StateT $ \s -> _
