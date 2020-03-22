import Data.Monoid
import Control.Applicative

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity 
  (<*>) (Identity f) = fmap f

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant c) = Constant c

instance Monoid a => Applicative (Constant a) where
  pure x = Constant mempty
  (<*>) x y = Constant ((getConstant x) `mappend` (getConstant y))
