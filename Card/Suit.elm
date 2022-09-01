module Poker.Card.Suit exposing (Suit(..), all, comparable, equals, fromChar, fromInt, make, order, show, toString)

import Poker.Card.Ranking exposing (Ranking(..))

type Suit
        = Spade
        | Heart 
        | Diamond 
        | Club

all : List Suit
all =
    [Spade, Heart, Diamond, Club]


make : Ranking -> Int -> Maybe Suit
make ranking card =
        case ranking of
                AceToTwo ->
                    case remainderBy 4 card of
                            0 -> Just Spade
                            1 -> Just Heart
                            2 -> Just Diamond
                            3 -> Just Club
                            _ -> Nothing

                SuitTwoToAce ->
                    case card // 4 of
                            0 -> Just Spade
                            1 -> Just Heart
                            2 -> Just Diamond
                            3 -> Just Club
                            _ -> Nothing

-- SHOW
fromChar : Char -> Maybe Suit
fromChar suit =
        case suit of
                's' -> Just Spade
                'h' -> Just Heart
                'd' -> Just Diamond
                'c' -> Just Club
                _ -> Nothing

fromInt : Int -> Suit
fromInt suit =
        case remainderBy 4 suit of
                0 -> Spade
                1 -> Heart
                2 -> Diamond
                _ -> Club

        
show : Suit -> String
show suit =
        case suit of
                Spade -> "\u{2660}"
                Heart -> "\u{2665}"
                Diamond -> "\u{2666}"
                Club -> "\u{2663}"

toString : Suit -> String
toString suit =
        case suit of
                Spade -> "s"
                Heart -> "h"
                Diamond -> "d"
                Club -> "c"



-- COMPARE
comparable : Suit -> Int
comparable suit =
        case suit of
                Spade -> 0
                Heart -> 1
                Diamond -> 2
                Club -> 3

order : Suit -> Suit -> Order
order lhs rhs =
        compare (comparable lhs) (comparable rhs)

equals : Suit -> Suit -> Bool
equals lhs rhs =
        (order lhs rhs) == EQ
