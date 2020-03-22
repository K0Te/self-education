{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- Type-level parser combinators for parsing type-level token streams
--
-- See this in action at https://github.com/mstksg/typelits-printf/blob/047682afaba97a4a67197a8deb44fb8ee83b87a7/src/GHC/Typelits/Printf/Parse.hs

import           Data.Kind (Type)
import           Data.Type.Bool (If)
import           Data.Type.Equality (type (==))

-- | Defunctionalized parser with stream of 'chr' tokens producing 'a'
type Parser chr a = chr -> a -> Type

type family RunParser (p :: Parser chr a) (str :: [chr]) :: Maybe (a, [chr])

-- consume all input, produce x ?
data Pure :: a -> Parser chr a
type instance RunParser (Pure x) str = 'Just '(x, str)

data Sym :: chr -> Parser chr chr
type instance RunParser (Sym c) (d ': cs) = If (c == d) ('Just '(c, cs)) 'Nothing
type instance RunParser (Sym c) '[]       = 'Nothing

data (<$) :: b -> Parser chr a -> Parser chr b
type family RepHelp (x :: b) (r :: Maybe (a, [chr])) :: Maybe (b, [chr]) where
    RepHelp x 'Nothing        = 'Nothing
    RepHelp x ('Just '(y, s)) = 'Just '(x, s)
type instance RunParser (x <$ p) str = RepHelp x (RunParser p str)

data (<|>) :: Parser chr a -> Parser chr a -> Parser chr a
type family ChoiceMaybe (x :: Maybe a) (y :: Maybe a) :: Maybe a where
    ChoiceMaybe ('Just x) y = 'Just x
    ChoiceMaybe 'Nothing  y = y
type instance RunParser (x <|> y) str = ChoiceMaybe (RunParser x str) (RunParser y str)

type Optional p = ('Just <$> p) <|> Pure 'Nothing

data (*>) :: Parser chr a -> Parser chr b -> Parser chr b
type family SeqHelp (p :: Parser chr b) (r :: Maybe (a, [chr])) :: Maybe (b, [chr]) where
    SeqHelp p 'Nothing          = 'Nothing
    SeqHelp p ('Just '(x, str)) = RunParser p str
type instance RunParser (x *> y) str = SeqHelp y (RunParser x str)

data (<$>) :: (a -> b) -> Parser chr a -> Parser chr b
type family MapConHelp (f :: a -> b) (r :: Maybe (a, [chr])) :: Maybe (b, [chr]) where
    MapConHelp f 'Nothing = 'Nothing
    MapConHelp f ('Just '(x, str)) = 'Just '(f x, str)
type instance RunParser (f <$> p) str = MapConHelp f (RunParser p str)

data (<*>) :: Parser chr (a -> b) -> Parser chr a -> Parser chr b
type family ApHelp (r :: Maybe (a -> b, [chr])) (q :: Parser chr a) :: Maybe (b, [chr]) where
    ApHelp 'Nothing q          = 'Nothing
    ApHelp ('Just '(f, str)) q = RunParser (f <$> q) str
type instance RunParser (p <*> q) str = ApHelp (RunParser p str) q

data Many :: Parser chr a -> Parser chr [a]
type instance RunParser (Many p) str = RunParser (Some p <|> Pure '[]) str
data Some :: Parser chr a -> Parser chr [a]
type instance RunParser (Some p) str = RunParser ('(:) <$> p <*> Many p) str
