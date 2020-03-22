module LearnParsers where

import Text.Trifecta
import Control.Applicative
import Control.Arrow
import Data.Ratio ((%))
import Data.Maybe
import Data.Char

stop :: Parser a
stop = unexpected "stop"

one = char '1' >> eof

one' = one >> stop

oneTwo = char '1' >> char '2'

oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

unip :: Parser String
unip = string "1" <|> string "12" <|> string "123"

mstring :: String ->  Parser Char
mstring "" = return 'a'
mstring (x:xs) = (char x) >> (mstring xs)

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

parseIntOrDec = (Left <$> (try parseFraction)) <|> (Right <$> decimal)


splitHalf :: [a] -> ([a],[a])
splitHalf xs = go xs xs
  where
    go (y:ys) (_:_:zs) = let (x1,x2) = (go ys zs) in (y:x1, x2)
    go ys _ = ([],ys)


-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
zip2 :: [a] -> [b] -> [(a,b)]
zip2 = foldr f b
  where
    f x k (y:ys) = (x,y) : k ys
    f x k [] = []
    b _ = []

zipRev xs ys = snd (foldr f (ys,[]) xs)
  where
    f x (y:ys,r) = (ys,(x,y):r)

data NumberOrString =
         NOSS String
       | NOSI Integer deriving (Eq, Show)
type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]
data SemVer =
  SemVer Major Minor Patch Release Metadata deriving Show

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- decimal
  char '.'
  minor <- decimal
  char '.'
  patch <- decimal
  pre <- optional $ do
    char '-'
    parseManyExtra
  meta <- optional $ do
    char '+'
    parseManyExtra
  eof
  return $ SemVer major minor patch (fromMaybe [] pre) (fromMaybe [] meta)

parseExtra :: Parser NumberOrString
parseExtra = do
  xs <- some alphaNum
  return $ if all isDigit xs
    then NOSI (read xs)
    else NOSS xs

parseManyExtra :: Parser [NumberOrString]
parseManyExtra =  parseExtra `sepBy` (symbol ".")

-- 2
parseDigit :: Parser Int
parseDigit = (read . pure) <$> oneOf "0123456789"

base10Integer :: Parser Int
base10Integer = do
  xs <- (some parseDigit) :: Parser [Int]
  return $ snd $ foldr (\x (m, v) -> (m*10, x*m+v)) (1, 0) xs

parseNeg :: Parser Int
parseNeg = do
  isNeg <- isJust <$> optional (symbol "-")
  val <- base10Integer
  return $ if isNeg then -val else val


