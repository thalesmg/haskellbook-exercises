import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import StateT
import ReaderT

newtype MaybeT m a = MaybeT {runMaybe :: m (Maybe a)}

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT ((fmap . fmap) f ma)

instance Applicative m => Applicative (MaybeT m) where
  pure a = MaybeT (pure (Just a))
  (MaybeT mf) <*> (MaybeT ma) = MaybeT $ (<*>) <$> mf <*> ma

instance Monad m => Monad (MaybeT m) where
  return = pure
  (MaybeT mma) >>= f =
    MaybeT $ do
      ma <- mma
      case ma of
        Just a -> runMaybe . f $ a
        Nothing -> return Nothing

instance MonadTrans MaybeT where
  lift ma = MaybeT (Just <$> ma)

instance MonadIO m => MonadIO (MaybeT m) where
  liftIO = lift . liftIO

instance MonadIO m => MonadIO (StateT s m) where
  liftIO io =
    StateT $ \s -> do
      a <- liftIO io
      return (a, s)

instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO
