newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma
  -- fmap f (ReaderT rma) = ReaderT $ \r -> f <$> (rma r)

instance Applicative m => Applicative (ReaderT r m) where
  pure = ReaderT . pure . pure
  (ReaderT f) <*> (ReaderT a) = ReaderT $ (<*>) <$> f <*> a

instance Monad m => Monad (ReaderT r m) where
  return = pure
  (ReaderT rma) >>= f =
    ReaderT $ do
      ma <- rma
      \r -> do
        a <- ma
        let rmb = runReaderT . f $ a
        rmb r
    -- ReaderT $ \r -> do
    --   a <- rma r
    --   runReaderT (f a) r
