module Poker.Evaluator exposing (Evaluator(..), evaluation, evaluations, foldl)

import Poker.Card.Card as Card exposing (Card)
import Poker.Evaluators.SKEval as SK

type Evaluator 
        = SKEval

evaluation : List Card -> Evaluator -> Maybe Int
evaluation cards evaluator =
        case evaluator of
                SKEval ->
                        SK.eval cards


evaluations : List Card -> Evaluator -> List (List Card, Int)
evaluations board evaluator =
        foldl board evaluator (\(cards, mEval) data -> Maybe.withDefault data (Maybe.map ((Tuple.pair cards) >> List.singleton >> ((++) data)) mEval)) []



foldl : List Card -> Evaluator -> ((List Card, Maybe Int) -> a -> a) -> a -> a
foldl board evaluator f start =
        let 
            cards = filterCards board
            target = 7 - (List.length board)
        in
            recFoldl target evaluator board cards [] f start 


recFoldl : Int -> Evaluator -> List Card -> List Card -> List Card -> ((List Card, Maybe Int) -> a -> a) -> a -> a
recFoldl target evaluator board cards current f data =
        case cards of
                [] -> data
                head :: tail ->
                        case List.length current == target of
                                True ->
                                        f (current, evaluation (board ++ current) evaluator) data
                                False ->
                                        data
                                        |> recFoldl target evaluator board tail (head :: current) f 
                                        |> recFoldl target evaluator board tail current f

filterCards : List Card -> List Card
filterCards board =
        Card.deck
        |> List.filter (\card -> not <| (List.member card board))
        
