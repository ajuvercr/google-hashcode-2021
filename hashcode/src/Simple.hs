module Simple
    ( printRoundRobin
    ) where

import Types
import qualified Data.Map as M

printIncoming :: Street -> IO ()
printIncoming Street { name=name } = do
    putStr name
    putStrLn " 2"

printIntersection :: Intersection -> IO ()
printIntersection (id, incoming, outgoing) = do
    print id
    print $ length incoming
    mapM_ printIncoming incoming

printRoundRobin :: StreetPlan -> IO ()
printRoundRobin (streets, intersections, _) = do
    print $ length intersections
    mapM_ printIntersection intersections
