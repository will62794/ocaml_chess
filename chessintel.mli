open Chesstypes
(*
	chessintel.mli

	interface to the OCamlChess AI engine

*)


(*Takes in a board and a difficulty level and returns a move*)
val request_move: difficulty_level -> game -> team -> move


