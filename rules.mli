(* 
	rules.mli

	rules.ml provides a chess rules engine that supports both the AI and board
	module, so that they are able to determine the validity of 
	moves and piece locations on a board

*)


(* 
	Given a board, representing the current state of a game,
	and a desired move determines whether that move is valid, 
	as governed by standard chess rules
*)
val valid_move : move -> board -> bool



