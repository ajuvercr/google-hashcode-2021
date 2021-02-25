{-# LANGUAGE TupleSections #-}

module StreetWeights
    ( streetWeights
    ) where


import Types
import qualified Data.Map as M

type Weights = M.Map Street Float

iterAmount :: Int
iterAmount = 100

initWeights :: StreetPlan -> Weights
initWeights (ss, _, _) = M.fromList $ zip ss (repeat 1)

streetWeights :: StreetPlan -> Weights
streetWeights p =
    let ws = iterate (updateWeights p) (initWeights p)
    in  ws !! iterAmount


updateWeights :: StreetPlan -> Weights -> Weights
updateWeights (_, is, _) w = M.fromList $ concatMap (someFunc w) is


someFunc :: Weights -> Intersection -> [(Street, Float)]
someFunc ws (_, ins, outs) =
    let totalIncoming = sum $ map (ws M.!) ins
        totalOutgoing = sum $ map (ws M.!) outs
        weightPerOut  = map (\x -> totalIncoming * (ws M.! x) / totalOutgoing) outs
    in  zip outs weightPerOut
