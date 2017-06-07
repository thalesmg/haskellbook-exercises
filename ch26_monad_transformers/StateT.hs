module StateT where

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
  fmap f (StateT smas) =
    StateT $ \s -> ff (smas s)
    where
      ff = fmap (\(a, s) -> (f a, s))

instance Monad m => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)
  (StateT smfabs) <*> (StateT smas) =
    StateT $ \s -> do
      (fab, s') <- smfabs s
      (a, s'') <- smas s'
      return $ (fab a, s'')

instance Monad m => Monad (StateT s m) where
  return = pure
  (StateT smas) >>= f =
    StateT $ \s -> do
      (a, s') <- smas s
      runStateT (f a) s'
