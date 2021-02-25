module Lib
    ( pizzaaa
    ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Control.Monad.State.Lazy (State)
import qualified Control.Monad.State.Lazy as State

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

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe _ (Just a) = a

type Teams = (Int, Int, Int) -- #2 persons, #3 persons, #4 persons
type Pizza = (Set Int, Int)
type Output = (Int, [Int]) -- teamsize, pizzas

getOrInsertDefault :: Ord k => k -> a -> Map k a -> (a, Map k a)
getOrInsertDefault key value map = let (ma, map') = Map.insertLookupWithKey f key value map in (fromMaybe value ma, map')
    where f _ _ old = old

ingId :: String -> State (Map String Int) Int
ingId s = State.state f
    where
        f map = getOrInsertDefault s (Map.size map) map


-- parsePizza :: (Map String Int, [[Int]]) -> [String] -> (Map String Int, [Set Int])
parsePizza :: [String] -> State (Map String Int) (Set Int)
parsePizza input = Set.fromList <$> mapM ingId input

parsePizzas :: [[String]] -> [Set Int]
parsePizzas inp = fst $ State.runState (mapM parsePizza inp) Map.empty

-- Parse file
parse :: String -> IO (Teams, [Pizza])
parse file = do
    (teams_line:pizzas) <- lines <$> readFile file
    let teams = triplet $ map read $ tail $ words teams_line
    let pizzaSets = parsePizzas $ map (tail . words) pizzas
    let pizzaaas = zip pizzaSets [0..]
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

optim :: Int -> [Pizza] -> ([Pizza], [Int], Set Int)
optim 0 ps = (ps, [], Set.empty)
optim x ps = (filter (/= (pizza_ing, pizza_id)) ops, pizza_id:res, Set.union ing pizza_ing)
    where
        (ops, res, ing) = optim (x-1) ps
        pizzaLen (pi, _) = Set.size $ Set.union pi ing
        (pizza_ing, pizza_id) = maxBy pizzaLen ops
