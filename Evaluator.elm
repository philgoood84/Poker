module Poker.Evaluator

import Poker.Evaluators.SKEval as SK

type Evaluator 
	= SKEval


evaluation : Evaluator -> List Int -> Maybe Int
evaluation evaluator cards =
	case evaluator of
		SKEval ->
			SK.eval cards


