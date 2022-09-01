module Poker.Card.Card exposing (Card(..), comparable, cons, deck, equals, face, fromChar, fromString, order, sameFace, sameSuit, show, suit, toString, uncons)

import Array
import Poker.Card.Face as Face exposing (Face)
import Poker.Card.Suit as Suit exposing (Suit)
import Poker.Card.Ranking exposing (Ranking(..))
import List.Extra as ListExtra

type Card =
        Card Face Suit

cons : Ranking -> Int -> Maybe Card
cons ranking card =
        Maybe.map2 (Card) (Face.make ranking card) (Suit.make ranking card)

uncons : Card -> (Face, Suit)
uncons card =
        (face card, suit card)

deck : List Card
deck =
        ListExtra.lift2 Card Face.all Suit.all

-- COMPARE
equals : Card -> Card -> Bool
equals c1 c2 =
        (comparable c1) == (comparable c2)

comparable : Card -> Int
comparable (Card f s) =
        (4 * (Face.comparable f)) + (Suit.comparable s)


order : Card -> Card -> Order
order lhs rhs =
        compare (comparable lhs) (comparable rhs)

sameSuit : Card -> Card -> Bool
sameSuit (Card _ s1) (Card _ s2) =
        (Suit.order s1 s2) == EQ

sameFace : Card -> Card -> Bool
sameFace (Card f1 _) (Card f2 _) =
        (Face.order f1 f2) == EQ

--  SHOW

fromString : String -> Maybe Card
fromString card =
        case (String.length card == 2, Array.fromList <| String.toList card) of
                (True, array) ->
                        Maybe.map2 Card (Maybe.andThen Face.fromChar (Array.get 0 array)) (Maybe.andThen Suit.fromChar (Array.get 1 array))
                _ ->
                        Nothing


fromChar : Char -> Char -> Maybe Card
fromChar f s =
        Maybe.map2 Card (Face.fromChar f) (Suit.fromChar s)

toString : Card -> String
toString card =
        uncons card
        |> Tuple.mapBoth Face.toString Suit.toString
        |> (\t -> (Tuple.first t) ++ (Tuple.second t))





show : Card -> String 
show card  =
        uncons card
        |> Tuple.mapBoth Face.show Suit.show
        |> (\t -> (Tuple.first t) ++ (Tuple.second t))


suit : Card -> Suit
suit (Card _ s) =
        s

face : Card -> Face
face (Card f _) =
        f




