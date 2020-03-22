-- Haskell ghc 8.6.3, lts-13.5 base-prelude, logict, pipes, hashtables, random, text, vector, aeson, lens, lens-aeson, split, bytestring, array, arrow-list, regex-applicative, regex-base, regex-compat, regex-pcre-builtin, regex-posix, regex-tdfa, parsec, unordered-containers, attoparsec, comonad, deepseq, dlist, either, matrix, MemoTrie, threads, monad-memo, memoize, base-unicode-symbols, basic-prelude, bifunctors


-- Enter your code here. Read input from STDIN. Print output to STDOUT
import Control.Monad (forM_, forM)
import Data.List (sort)

main = do
    t <- readLn :: IO Int
    forM_ [1..t] $ \_ -> do
        n <- readLn :: IO Int
        xys <- forM [1..n] $ \_ -> do
            s <- getLine
            let [x, y] = (map read $ words s) :: [Int]
            return (x,y)
        putStrLn (if is_func xys then "YES" else "NO")

is_func :: [(Int, Int)] -> Bool
is_func l = let xs = (fst <$> sort l) in not $ any id $ zipWith (==) xs (tail xs)


-- Enter your code here. Read input from STDIN. Print output to STDOUT

import Control.Monad (forM_, forM, guard)
import Data.List (sort)
import Data.Maybe

main = do
    t <- getLine
    s <- getLine
    let [n, _] = (map read $ words t) :: [Int]
    let xs = (map read $ words s) :: [Int]
    let res = solve 1 n xs
    maybe (putStrLn "-1") printSol res

printSol :: [Int] -> IO ()
printSol xs = do
    let st  = sort xs
    let res = scanl (\a b -> a*b) 1 st
    putStrLn $ unwords $ show <$> res

solve :: Int -> Int -> [Int] -> Maybe [Int]
solve c n xs = do
    let nxs = reverse $ sort $ filter (\x -> ((n `mod` x) == 0) && n >= x && x /= 1) xs
    if c == n
        then Just []
        else do guard $ nxs /= []
                res <- solve c (n `div` (head nxs)) nxs
                return $ head nxs : res


import Control.Monad (forM_, forM, guard)
import Data.List (sort, foldl')
import Data.Maybe
import Data.DList (empty, DList, toList)
import Data.Vector (fromList, Vector)

main = do
    s <- getLine
    let [m_max, n_max, r] = (map read $ words s) :: [Int]
    ass <- forM [1..m_max] $ \m -> do
        st <- getLine
        let as = (map read $ words st) :: [Int]
        return as
    let circles_cnt = ((min m_max n_max) + 1) `div` 2
    let initial_dlists = take circles_cnt $ repeat empty
    let (dcircles, _) = foldl' (\(dlists, m) line -> (mkCircles dlists m m_max line, m+1)) (initial_dlists, 0) ass
    let circles = normalize m_max n_max $ (fromList . Data.DList.toList) <$> dcircles
    let rcircles = rotate circles
    putStrLn "?"

normalize :: Int -> Int -> [Vector Int] -> [Vector Int]
normalize = undefined

rotate :: r -> [Vector Int] -> [Vector Int]
rotate = undefined

mkCircles :: [DList Int] -> Int -> Int -> [Int] -> [DList Int]
mkCircles dlists m m_max line = undefined

circle_cnt m m_max = (m_max `div` 2 -  ( abs $ m -  m_max `div` 2))



import Control.Monad (forM_, forM, guard)
import Data.List (sort, foldl', concat)
import Data.Maybe
import Data.DList (empty, DList, toList)
import Data.Vector (fromList, Vector, backpermute, enumFromN, slice, toList)

main = do
    s <- getLine
    let [m_max, n_max, r] = (map read $ words s) :: [Int]
    ass <- forM [1..m_max] $ \m -> do
        st <- getLine
        let as = (map read $ words st) :: [Int]
        return as
    let newcoords = rotate m_max n_max r
    let matrix = (fromList . concat) ass
    printSl (backpermute matrix newcoords) m_max n_max

data Circle = Circle Int Int Int Int deriving (Eq, Show)

mkCircles :: Int -> Int -> [Circle]
mkCircles m n = go m n 0
    where go m n x | m == 0 = []
                   | n == 0 = []
                   | otherwise = (Circle x x m n) : (go (m-2) (n-2) (x+1))  

incircle :: Circle -> Int -> Int -> Bool
incircle (Circle offx offy sy sx) x y
    | y == offy && x >= offx && x < (offx + sx) = True
    | y == (offy + sy - 1) && x >= offx && x < (offx + sx) = True
    | x == offx && y >= offy && y < (offy + sy) = True
    | x == (offx + sx - 1) && y >= offy && y < (offy + sy) = True
    | otherwise = False

rotateInCircle :: Circle -> Int -> Int -> Int -> (Int, Int)
rotateInCircle c@(Circle offx offy  sy sx) x y r
    | r >= (2*sx + 2 * sy -4) = rotateInCircle c x y (r `mod` (2*sx + 2 * sy -4))
    | r == 0 = (x, y)
    | y == offy && x >= offx && x < (offx + sx - 1) = rotateInCircle c (x+1) y (r-1)
    | y == (offy + sy - 1) && x >= (offx + 1) && x < (offx + sx) = rotateInCircle c (x-1) y (r-1)
    | x == offx && y >= (offy+1) && y < (offy + sy) = rotateInCircle c (x) (y-1) (r-1)
    | x == (offx + sx - 1) && y >= offy && y < (offy + sy) = rotateInCircle c (x) (y+1) (r-1)

rotate :: Int -> Int -> Int -> Vector Int
rotate m n r = coordToSrc <$> enumed
    where
        enumed = enumFromN 0 (m*n)
        coordToSrc x = let xy = xToXY x in xyToX $ rotateInCircle (xToCircle xy) (fst xy) (snd xy) r 
        xToCircle (x, y) = head $ filter (\c -> incircle c x y) circles
        circles = mkCircles m n
        xToXY x = let (y1,x1) = (x `divMod` n) in (x1,y1)
        xyToX (x, y) = y*n + x


printSl :: Vector Int -> Int -> Int -> IO ()
printSl res m n  = do
    forM_ [0..(m-1)] $ \x -> do
        let vec = slice (x*n) n res
        putStrLn $ unwords $ map show $ Data.Vector.toList vec

