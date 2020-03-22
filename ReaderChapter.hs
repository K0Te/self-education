module ReaderChapter where

import Control.Applicative (liftA2)

newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person {
  humanName :: HumanName , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)
data Dog = Dog {
  dogsName :: DogName
  , dogsAddress :: Address } deriving (Eq, Show)

pers :: Person
pers = Person (HumanName "Big Bird") (DogName "Barkley")
  (Address "Sesame Street")
chris :: Person
chris = Person (HumanName "Chris Allen") (DogName "Papu")
  (Address "Austin")

getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

getDogR' :: Person -> Dog
getDogR' = liftA2 Dog dogName address

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = pure f <*> fa <*> fb

newtype Reader r a = Reader { runReader :: r -> a }

asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ \r -> f (ra r)

instance Applicative (Reader r) where
  pure a = Reader $ \x -> a
  (Reader rab) <*> (Reader ra) = Reader $ \r -> (rab r) (ra r)

instance Monad (Reader r) where
  return a = Reader $ \x -> a
  (Reader ra) >>= fa_rb = Reader $ \r -> runReader (fa_rb (ra r)) r

getDogRM :: Reader Person Dog
getDogRM = do
  name <- Reader dogName
  addy <- Reader address
  return $ Dog name addy





