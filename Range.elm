module Poker.Range exposing (Range, empty, holes, get, map, andMap, member, merge, toList)

import Dict exposing (Dict)
import List.Extra as ListExtra
import Poker.Hole as Hole exposing (Hole)
import Poker.Game as Game exposing (Game)
import Tuple 

type alias Range a = Dict String a 

empty : Range a
empty = 
        Dict.empty

get : Hole -> Range a -> Maybe a
get hole =
        Dict.get (Hole.toString hole) 

member : Hole -> Range a -> Bool
member hole =
        Dict.member (Hole.toString hole)

holes : Range a -> List Hole
holes range =
        range
        |> Dict.keys
        |> List.map Hole.fromString 

toList : Range a -> List (Hole, a)
toList range =
        Dict.toList range
        |> List.map (Tuple.mapFirst Hole.fromString)

merge : (a -> b -> c) -> Range a -> Range b -> Range c
merge f first second =
        Dict.merge 
                (\_ _ -> identity)
                (\hole m1 m2 -> Dict.insert hole (f m1 m2))
                (\_ _ -> identity)
                first
                second
                empty


map : (a -> b) -> Range a -> Range b
map f =
        Dict.map (\_ -> f)

andMap : (a -> Maybe b) -> Range a -> Range b
andMap f range =
        Dict.foldl (\hole value dict -> 
                case f value of
                        Nothing -> dict
                        Just fvalue ->
                                Dict.insert hole fvalue dict
                   ) empty range


