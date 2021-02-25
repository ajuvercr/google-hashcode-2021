module Lib
    ( pizzaaa
    ) where

import Data.Set (Set)
import qualified Data.Set as Set
import System.IO
import System.Environment

-- Get filename
pizzaaa :: IO ()
pizzaaa = do
    file <- head <$> getArgs
    input <- parse file
    output (heuristic input)


triplet :: [a] -> (a,a,a)
triplet [a,b,c] = (a,b,c)

mcons :: Maybe a -> [a] -> [a]
mcons (Just x) xs = x:xs
mcons _ xs = xs


maxBy :: Ord a => (b -> a) -> [b] -> b
maxBy _ [a] = a
maxBy f (x:xs) = let b = maxBy f xs in
        if f b > f x
        then b
        else x


type Teams = (Int, Int, Int) -- #2 persons, #3 persons, #4 persons
type Pizza = (Set String, Int)
type Output = (Int, [Int]) -- teamsize, pizzas

-- Parse file
parse :: String -> IO (Teams, [Pizza])
parse file = do
    (teams_line:pizzas) <- lines <$> readFile file
    let teams = triplet $ map read $ tail $ words teams_line
    let parsePizza = Set.fromList . tail . words
    let pizzaaas = zip (map parsePizza pizzas) [1..]
    return (teams, pizzaaas)

output :: [Output] -> IO ()
output os = do
    print $ length os
    foldMap printPizza os
    where
        printPizza :: Output -> IO ()
        printPizza (t, ps) = putStrLn $ unwords (show t : map show ps)


heuristic :: (Teams, [Pizza]) -> [Output]
heuristic (_, []) = []
heuristic ((ones, twos, trees), ps) | trees > 0 && length ps >= 4 = let (rest, ids, _) = optim 4 ps in
    (4, ids):heuristic ((ones, twos, trees-1), rest)
heuristic ((ones, twos, 0), ps) | twos > 0 && length ps >= 3 = let (rest, ids, _) = optim 3 ps in
    (3, ids):heuristic ((ones, twos-1, 0), rest)
heuristic ((ones, 0, 0), ps) | ones > 0 && length ps >= 2 = let (rest, ids, _) = optim 2 ps in
    (2, ids):heuristic ((ones-1, 0, 0), rest)
heuristic _ = []

optim :: Int -> [Pizza] -> ([Pizza], [Int], Set String)
optim 0 ps = (ps, [], Set.empty)
optim x ps = (filter (/= (pizza_ing, pizza_id)) ops, pizza_id:res, Set.union ing pizza_ing)
    where
        (ops, res, ing) = optim (x-1) ps
        pizzaLen (pi, _) = Set.size $ Set.union pi ing
        (pizza_ing, pizza_id) = maxBy pizzaLen ops
