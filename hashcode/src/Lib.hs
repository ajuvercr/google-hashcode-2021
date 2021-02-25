module Lib
    ( someFunc
    ) where

import Simple
import Types
import StreetWeights
import SWOut

import Data.Function
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Control.Monad.State.Lazy (State)
import qualified Control.Monad.State.Lazy as State

import System.IO
import System.Environment

-- Get filename
someFunc :: IO ()
someFunc = do
    file <- head <$> getArgs
    input <- parse file
    -- printRoundRobin input
    let ws = streetWeights input
    printOutput ws input
        -- output (heuristic input)


slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)


modifyF :: Street -> IntersectionBuilder -> IntersectionBuilder
modifyF s map = Map.adjust f1 (eInter s) $ Map.adjust f2 (sInter s) map
    where
        f1 (xs, ys) = (s:xs, ys)
        f2 (xs, ys) = (xs, s:ys)


parseStreet :: Int -> String -> State IntersectionBuilder Street
parseStreet id inp = do
    let [start, end, name, duration] = words inp
    let street = Street (read start) (read end) (read duration) name
    State.modify' $ modifyF street
    return street

type IntersectionBuilder = Map Int ([Street], [Street])

getIntersections :: [(Int, ([Street], [Street]))] -> [Intersection]
getIntersections = map $ uncurry f
    where f id (input, output) = (id, input, output)

-- Parse file
parse :: String -> IO StreetPlan
parse file = do
    (head:ls) <- lines <$> readFile file
    let [duration, intersections, street_count, cars, bonus] = map read $ words head :: [Int]
    let intersectionMap = Map.fromList $ zip [0..(intersections - 1)] (repeat ([], []))
    let streets_strings = slice 0 (street_count - 1) ls
    let (streets, state) = State.runState (State.zipWithM parseStreet [0 .. ] streets_strings) intersectionMap
    return (streets, Map.assocs state & getIntersections, [])
