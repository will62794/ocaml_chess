open Chesstypes
open Chessmodel

(*
	rules.mli

	rules.ml provides a chess rules engine that supports both the AI and board
	module, so that they are able to determine the validity of
	moves and piece locations on a board

*)


(*
	Given a game and a desired move, determines whether that move is valid,
	as governed by standard chess rules, and returns the validation type of that 
	move, if valid
*)
val valid_move : move -> game -> move_validation

(* 
	Get all possible moves for piece on the board
  	as well as opponent's vulnerabilities, given a game state
*)
val possible_movements: piece -> game -> (move * movetype) list

(* 
	Given a list of valid moves, determines all of the opponent pieces that would are capturable
 	by those moves. Assumes that all of the moves in move list are moves of the same piece
*)
val pieces_capturable_by_moves: (move * movetype) list -> board -> piece list
