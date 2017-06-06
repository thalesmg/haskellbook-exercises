newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT mea) = EitherT ((fmap . fmap) f mea)

instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT . pure . pure
  (EitherT f) <*> (EitherT a) = EitherT ((<*>) <$> f <*> a)

instance Monad m => Monad (EitherT e m) where
  return = pure
  (EitherT mea) >>= f =
    EitherT $ do
      ea <- mea
      case ea of
        Left e  -> return . Left $ e
        Right r -> runEitherT $ f r

swapEither :: Either a b -> Either b a
swapEither (Left a) = Right a
swapEither (Right b) = Left b

swapEitherT :: Functor m => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mea) = EitherT $ swapEither <$> mea

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT mab) =
  do
    eab <- mab
    case eab of
      Left a  -> f a
      Right b -> g b
