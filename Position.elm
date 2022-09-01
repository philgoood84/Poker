module Poker.Position exposing (Position(..), all, comparable, equals, next, order, show)

type Position
        = UTG
        | HJ
        | CO
        | BU
        | SB
        | BB
        

all : List Position
all =
        [ UTG, HJ, CO, BU, SB, BB ]


show : Position -> String
show position =
        case position of
                UTG -> "UTG"
                HJ -> "HJ"
                CO -> "CO"
                BU -> "BU"
                SB -> "SB"
                BB -> "BB"


comparable : Position -> Int
comparable position =
        case position of
                UTG -> 2 
                HJ -> 3 
                CO -> 4 
                BU -> 5 
                SB -> 0 
                BB -> 1 

order : Position -> Position -> Order
order lhs rhs =
        compare (comparable lhs) (comparable rhs)


next : Position -> Position
next position =
        case position of
                UTG -> HJ
                HJ -> CO
                CO -> BU
                BU -> SB
                SB -> BB
                BB -> UTG

equals : Position -> Position -> Bool
equals lhs rhs =
        (comparable lhs) == (comparable rhs)
