open Chesstypes

(*
	display.mli

	display.ml handles the production of good looking ASCII output given some
	part of the internal data model of the game (board, player, etc.)

*)

val print_board: board -> unit

val print_total_moves: int -> unit
