module Poker.Hole exposing (Hole, all, blocked, cons, fromString, overlap, pair, toString, show)

import Bitwise
import Dict exposing (Dict)
import List.Extra as ListExtra
import Poker.Game as Game exposing (Game)
import Poker.Card.Card as Card exposing (Card(..))
import Poker.Card.Face as Face exposing (Face)
import Poker.Card.Suit as Suit exposing (Suit)
import String.Extra as StringExtra
import Tuple

type alias Hole = List Card

cons : List Card -> Hole
cons cards =
        List.sortBy Card.comparable cards 

pair : (Card, Card) -> Hole
pair (c1, c2) =
        cons [ c1, c2 ]

all : Game -> List Hole
all game =
        case game of
                Game.Holdem ->
                        ListExtra.uniquePairs Card.deck
                        |> List.map (\p -> [ Tuple.first p, Tuple.second p ])
                        |> List.map cons 

fromString : String -> Hole
fromString cards =
        StringExtra.break 2 cards
        |> List.filterMap Card.fromString
        |> cons 

toString : Hole -> String
toString hole =
        hole
        |> ListExtra.reverseMap (Card.toString)
        |> List.foldl (++) ""


show : Hole -> String
show hole =
        hole
        |> ListExtra.reverseMap (Card.show)
        |> List.foldl (++) ""

blocked : Card -> Hole -> Bool
blocked card hole  =
        hole
        |> List.foldl ((Card.equals card) >> (||)) False

overlap : Hole -> Hole -> Bool
overlap lhs rhs =
        List.foldl (\card -> (||) (blocked card rhs)) False lhs


