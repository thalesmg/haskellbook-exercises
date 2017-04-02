module Ex1 where

import Data.Traversable
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes (traversable)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)


-- IDENTITY


newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Foldable Identity where
  foldr f z (Identity x) = f x z

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = arbitrary >>= return . Identity

instance EqProp a => EqProp (Identity a) where
  (Identity x) =-= (Identity y) = x =-= y


-- CONSTANT

newtype Constant a b = Constant { getConstant :: a } deriving (Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldr _ z _ = z

instance Traversable (Constant a) where
  traverse _ (Constant a) = pure $ Constant a

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = arbitrary >>= pure . Constant

instance (EqProp a) => EqProp (Constant a b) where
  (Constant x) =-= (Constant y) = x =-= y



-- S??

data S n a = S (n a) a deriving (Show)

instance Functor n => Functor (S n) where
  fmap f (S na a) = S (f <$> na) (f a)

instance Foldable n => Foldable (S n) where
  -- foldr f z (S na a) = foldr f z na
  foldMap f (S na a) = foldMap f na `mappend` f a

instance Traversable n => Traversable (S n) where
  traverse f (S na a) = S <$> traverse f na <*> f a

instance (Arbitrary a, Applicative n) => Arbitrary (S n a) where
  arbitrary = arbitrary >>= \a -> return $ S (pure a) a

instance EqProp a => EqProp (S n a) where
  (S na a) =-= (S nb b) = a =-= b



  
type TInd = Identity
type TConst = Constant
type TS n = S n

main = do
  let triggerI = undefined :: TInd (Int, Int, [Int])
      triggerC = undefined :: TConst String (Int, Int, [Int])
      triggerS = undefined :: TS Maybe (Int, Int, [Int])
  quickBatch (traversable triggerI)
  quickBatch (traversable triggerC)
  quickBatch (traversable triggerS)
