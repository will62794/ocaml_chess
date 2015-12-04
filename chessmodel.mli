open Chesstypes
(*
	chessmodel.mli

	chessmodel provides the primitive operations for
	upating, modifying the current state of a game board.
	Initializing pieces, making moves, etc. It has no
	built-in knowledge of game rules. It tries to roughly
	model a true, physical board.

*)


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
	if move is invalid, returns None.
*)
val execute_move: move -> board -> board option

(* returns the ((src_x,src_y),(dest_x,dest_y)) of a move *)
val coords_of_move: move -> (int*int)*(int*int)

val get_int_of_letter: string -> int

val get_nth_letter: int -> string

val get_square_on_board: boardpos -> board -> square ref

(* returns the boardpos for the given piece; if piece not found, return None *)
val find_piece_pos: piece -> board -> boardpos option

(* returns all pieces, of any team, currently on given board *)
val all_board_pieces: board -> piece list

(* given a team and a board, return a list of all pieces belonging to that team currently present on board *)
val all_team_pieces: board -> team -> piece list

(* converts a boardpos into integer x,y board coordinates *)
val boardpos_to_coords: boardpos -> (int*int)

(* converts integer x,y coordinates into boardpos format*)
val coords_to_boardpos: (int*int) -> boardpos

(* creates a new piece from given attributes *)
val create_piece: string -> team -> string -> piecekind -> piece

(* creates a new piece from given attributes and places it on board at given position *)
val add_piece_to_board: board -> boardpos -> string -> team -> string -> piecekind -> unit

(*
	takes a board position and inverts each of its coordinates, so as to view
	the coordinate from the opponent's viewpoint  e.g. invert_board_pos ("2","b") = ("7","g")
*)
val invert_boardpos: boardpos -> boardpos

(*
	takes a board and reflects its coordinate labels horizontally and vertically
	physically, this would correspond to spinning the board around and viewing it from
	your opponent's perspective
*)
val reflect_board_coords: board -> board

val get_all_pieces: team-> board-> piece list

val get_piece: board -> boardpos -> piece

(* returns a board with same data but fresh references to each square *)
val copy_board: board -> board


val put_piece_at_boardpos: piece option -> boardpos ->  board -> unit

