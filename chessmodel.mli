
(* 
	chessmodel.mli

	chessmodel provides the primitive operations for 
	upating, modifying the current state of a game board.
	Initializing pieces, making moves, etc. It has no 
	built-in knowledge of game rules. It tries to roughly 
	model a true, physical board.

*)

type board
type move
type piece
type square

(* 
	returns a board with pieces arranged in standard
	starting chess positions. 
*)
val make_init_board: unit -> board


(* 
	returns a board where every square is empty analogous to a 
	physical chess board with no pieces on it. 
*)
val make_empty_board: unit -> board

(* 
	executes a move and returns a board option.
	the returned board will represent the updated state
	of the board after the move was executed. moves correspond to 
	physical moves, not moves that are checked against chess rules.
*)
val execute_move: move -> board -> board option 




