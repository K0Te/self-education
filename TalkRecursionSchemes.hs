{-# LANGUAGE DeriveFunctor #-}
module TalkRecursionSchemes where
-- Amazing talk !
-- https://www.youtube.com/watch?v=OwppikrRlOk
-- slides have readable text: https://slides.com/alexanderkonovalov-1/recursion-1/#/0/37
-- Talk is in Scala, examples re-implemented in haskell ^__^

-- (a + b) -> c ~~ (a->c, b->c)
-- c^(a+b) = c^a + c^b
fromEither :: (Either a b -> c) -> (a -> c, b -> c)
fromEither fe = (\a -> fe (Left a), \b -> fe (Right b))
fromTuple :: (a -> c, b -> c) -> (Either a b -> c)
fromTuple (fa, fb) (Left a) = fa a
fromTuple (fa, fb) (Right b) = fb b

-- c^b^a = c^(a*b)
unCurry :: (a -> b -> c) -> (a, b) -> c
unCurry f = (\(a, b) -> f a b)

-- Fold as basic case of recursion
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
-- map :: (a -> b) -> [a] -> [b]
foldToMap :: (a -> b) -> [a] -> [b]
foldToMap f = foldr (\a acc -> (f a):acc) []
foldToSum :: (Foldable t, Num a) => t a -> a
foldToSum = foldr (+) 0
-- Tricky, step can actually consume more than 2 arguments
-- and even pass extra arguments to embedded step calls (!)
foldToZip :: [a] -> [b] -> [(a, b)]
foldToZip l1 l2 = foldr step done l1 l2
  where
    done l2 = []
    step a f [] = []
    step a f (e:es) = (a,e):(f es)

-- Algebra
-- In category theory - F-algebra
-- F c -> c, where F is a signature (pattern functor) and c is carrier type
data Expr = Lit Int | Add Expr Expr | Mul Expr Expr
--               Lit -> Z     Z+Z -> Z         Z*Z -> Z
calc :: Expr -> (Int -> z) -> (z -> z -> z) -> (z -> z -> z) -> z
calc (Lit x) lit add mul = lit x
calc (Add x y) lit add mul = add (calc x lit add mul) (calc y lit add mul)
calc (Mul x y) lit add mul = mul (calc x lit add mul) (calc y lit add mul)

-- recursion scheme ?
data ExprR r = LitR Int | AddR r r | MulR r r deriving (Functor)

calcR :: Expr -> (ExprR z -> z) -> z
calcR e algebra = case e of
  (Lit x) -> algebra (LitR x)
  (Add x y) -> algebra (AddR (calcR x algebra) (calcR y algebra))
  (Mul x y) -> algebra (MulR (calcR x algebra) (calcR y algebra))


project :: Expr -> ExprR Expr
project (Lit x) = LitR x
project (Add x y) = AddR x y
project (Mul x y) = MulR x y

embed :: ExprR Expr -> Expr
embed (LitR x) = Lit x
ember (AddR x y) = Add x y
ember (MulR x y) = Mul x y

calcR2 :: Expr -> (ExprR z -> z) -> z
calcR2 e algebra = algebra mp where
  pre = (project e)
  mp = fmap (\x -> calcR2 x algebra) pre


