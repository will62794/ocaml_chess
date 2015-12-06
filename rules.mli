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
val valid_move : ?checks:bool -> move -> game -> move_validation

(* 
	Get all possible moves for piece on the board
  	as well as opponent's vulnerabilities, given a game state
*)
val possible_movements: ?checks:bool -> piece -> game -> (move * movetype) list

(* 
	Given a list of valid moves, determines all of the opponent pieces that would are capturable
 	by those moves. Assumes that all of the moves in move list are moves of the same piece
*)
val pieces_capturable_by_moves: (move * movetype) list -> board -> piece list


(* 
  Determines whether the king of the given team is in check or checkmate.
  returns Some(Check) if in check, Some(Checkmate) if in checkmate, or None
  if neither. Will return None if the specified king isn't on the board 
*)
val king_in_check: team -> game -> checktype option


val would_be_check: movetype -> move -> game -> bool

