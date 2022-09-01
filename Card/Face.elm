module Poker.Card.Face exposing (Face(..), all, comparable, equals, fromChar, make, order, show, toString)

import Poker.Card.Ranking exposing (Ranking(..))

type Face
        = Two 
        | Three
        | Four 
        | Five
        | Six
        | Seven
        | Eight
        | Nine
        | Ten
        | Jack
        | Queen
        | King
        | Ace


all : List Face
all =
    [Ace, King, Queen, Jack, Ten, Nine, Eight, Seven, Six, Five, Four, Three, Two]


make : Ranking -> Int -> Maybe Face
make ranking card =
        case ranking of
                AceToTwo ->
                        case remainderBy 13 card of
                                0 -> Just Two
                                1 -> Just Three
                                2 -> Just Four
                                3 -> Just Five
                                4 -> Just Six
                                5 -> Just Seven
                                6 -> Just Eight
                                7 -> Just Nine
                                8 -> Just Ten
                                9 -> Just Jack
                                10 -> Just Queen
                                11 -> Just King
                                12 -> Just Ace         
                                _ -> Nothing

                SuitTwoToAce ->
                    case card // 13 of
                            0 -> Just Ace
                            1 -> Just King
                            2 -> Just Queen
                            3 -> Just Jack
                            4 -> Just Ten
                            5 -> Just Nine
                            6 -> Just Eight
                            7 -> Just Seven
                            8 -> Just Six
                            9 -> Just Five
                            10 -> Just Four 
                            11 -> Just Three
                            12 -> Just Two 
                            _ -> Nothing

-- SHOW

fromChar : Char -> Maybe Face
fromChar face =
        case face of
                'A' -> Just Ace
                'K' -> Just King
                'Q' -> Just Queen
                'J' -> Just Jack
                'T' -> Just Ten
                '9' -> Just Nine
                '8' -> Just Eight
                '7' -> Just Seven
                '6' -> Just Six
                '5' -> Just Five
                '4' -> Just Four
                '3' -> Just Three
                '2' -> Just Two
                _ -> Nothing


show : Face -> String
show face =
        case face of
                Ace -> "A"
                King -> "K"
                Queen -> "Q"
                Jack -> "J"
                Ten -> "T"
                Nine -> "9"
                Eight -> "8"
                Seven -> "7"
                Six -> "6"
                Five -> "5"
                Four -> "4"
                Three -> "3"
                Two -> "2"

toString : Face -> String
toString face = 
        show face

-- COMPARE
comparable : Face -> Int
comparable face =
        case face of
                Ace -> 0 
                King -> 1
                Queen -> 2
                Jack -> 3
                Ten -> 4
                Nine -> 5
                Eight -> 6
                Seven -> 7
                Six -> 8
                Five -> 9
                Four -> 10
                Three -> 11
                Two -> 12

order : Face -> Face -> Order
order lhs rhs =
        compare (comparable lhs) (comparable rhs)


equals : Face -> Face -> Bool
equals lhs rhs =
        (comparable lhs) == (comparable rhs)
