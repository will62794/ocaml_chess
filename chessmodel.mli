
(* 
	board.mli

	board.ml provides the primitive operations for 
	upating, modifying the current state of a game board.
	Initializing pieces, making moves, etc. It does not
	have any sense of chess rules built in.

*)

type board
type move
type piece
type square

(* Makes initial board with chess pieces in correct spot *)
val make_init_board: unit -> board

(* returns a board with all squares empty *)
val make_empty_board: unit -> board

(* Executes a valid move and returns Some board. If move not valid return None *)
val execute_move: move -> board -> board option 




