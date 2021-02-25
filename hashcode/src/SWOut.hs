{-# LANGUAGE TupleSections #-}

module SWOut
    ( printOutput
    ) where


import StreetWeights
import Types
import qualified Data.Map as M

type Weights = M.Map Street Float

------------ HANDLE OUTPUT ------------

minAll :: Ord a => [a] -> a
minAll (x:xs) = foldl min x xs


clamp min max n
    | n < min = min
    | n > max = max
    | otherwise = n


printIncoming :: Street -> Float -> IO ()
printIncoming Street { name=name } strength = do
    let rs = clamp 1 100 (round strength)
    putStr name
    putStr " "
    print rs

printIntersection :: Weights -> Intersection -> IO ()
printIntersection ws (id, incoming, outgoing) = do
    print id
    print $ length incoming
    let weights = map (ws M.!) incoming
    let smallest = minAll weights
    let newWeights = map (/ smallest) weights
    mapM_ (uncurry printIncoming) (zip incoming newWeights)

printOutput :: Weights -> StreetPlan -> IO ()
printOutput ws (streets, intersections, _) = do
    print $ length intersections
    mapM_ (printIntersection ws) intersections
