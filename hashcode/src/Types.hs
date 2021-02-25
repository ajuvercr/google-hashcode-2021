module Types
    ( Street(..)
    , Intersection
    , StreetPlan
    , KobeStreet
    ) where

type KobeStreet = ()

-- | List of streets
type Car = [Street]

-- | (Start, End, Time, Name)
data Street = Street
    { sInter :: Int
    , eInter :: Int
    , time :: Int
    , name :: String
    } deriving (Show, Eq, Ord)

-- | (Id, Incoming, Outgoing)
type Intersection = (Int, [Street], [Street])

-- | Input State
type StreetPlan = ([Street], [Intersection], [Car])
