import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class

embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = MaybeT $ ExceptT $ ReaderT $ fmap return (const (Right (Just 1)))
-- b0 -> Either a0 (Maybe Integer)
-- -> b0 (Either a0 (Maybe Integer))
-- ReaderT :: (r -> m a) -> ReaderT r m a
-- ReaderT :: (-> r (m a)) -> ReaderT r m a
-- ExceptT :: m (Either e a) -> ExceptT e m a
-- MaybeT :: m (Maybe a) -> MaybeT m a
-- (r -> Either String (Maybe Integer))
