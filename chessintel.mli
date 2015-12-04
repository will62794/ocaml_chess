open Chess
(*
	chessintel.mli

	interface to the CamlChess AI engine

*)

(* edit this interface *)


(*Takes in a board and a difficulty level and returns a move*)
val request_move: difficulty_level -> board -> move


