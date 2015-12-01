open Chesstypes
open Chessmodel

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

(* 
	Get all possible moves for piece on the board
  	as well as opponent's vulnerabilities
*)
val possible_movements: piece -> board-> move list

(* 
	Given a list of valid moves, determines all of the opponent pieces that would are capturable
 	by those moves. Assumes that all of the moves in move list are moves of the same piece
*)
val pieces_capturable_by_moves: move list -> board -> piece list
