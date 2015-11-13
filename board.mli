(* 
	board.mli

	board.ml provides the primitive operations for 
	upating, modifying the current state of a game board.
	Initializing pieces, making moves, etc. It does not
	have any sense of chess rules built in.

*)

(* edit this interface *)


(*Makes intial board with chess pieces in correct spot*)
val make_init_board () -> board

(*Executes a valid move and returns Some board. If move not valid return None*)
val execute_move move->board->board option 