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

(* Get all possible moves for piece on the board
 * as well as opponent's vulnerabilities
 *)
val possible_movements: piece -> board-> (move list * piece list)

(* Returns: the board location is vulnerable to the opponent *)
val is_vulnerable_pos: boardpos -> board -> bool

(* Returns: the move leads to a vulnerable piece *)
val is_vulnerable_move: move -> board -> bool

