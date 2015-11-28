
(*

[ BOARD SQUARE ADDRESSING ]

	-------- Black --------
	a8 b8 c8 d8 e8 f8 g8 h8
	a7 b7 c7 d7 e7 f7 g7 h7
	a6 b6 c6 d6 e6 f6 g6 h6
	a5 b5 c5 d5 e5 f5 g5 h5
	a4 b4 c4 d4 e4 f4 g4 h4
	a3 b3 c3 d3 e3 f3 g3 h3
	a2 b2 c2 d2 e2 f2 g2 h2
	a1 b1 c1 d1 e1 f1 g1 h1
	-------- White --------

	and numerical (easier for move computations):
	-------- Black --------
	18 28 38 48 58 68 78 88
	17 27 37 47 57 67 77 87
	16 26 36 46 56 66 76 86
	15 25 35 45 55 65 75 85
	14 24 34 44 54 64 74 84
	13 23 33 43 53 63 73 83
	12 22 32 42 52 62 72 82
	11 21 31 41 51 61 71 81
	-------- White --------

Addressing a square:

	(row,column)
	e.g. ("5","a")

 	board is an association list that maps (row_index:string -> row:row)
	row is an association lists that maps (col_index:string -> square ref)


[ PIECE LABELINGS ]


	------- Black ---------
	R2 K2 B2 Q  K  B1 K1 R1
	P8 P7 P6 P5 P4 P3 P2 P1
	-- -- -- -- -- -- -- --
	-- -- -- -- -- -- -- --
	-- -- -- -- -- -- -- --
	-- -- -- -- -- -- -- --
	P1 P2 P3 P4 P5 P6 P7 P8
	R1 K1 B1 Q  K  B2 K2 R2
	------- White ---------

	* each piece will also have
	a "team" associated with it i.e. Black | White


*)

type team = White | Black

type piecekind =
	| Pawn
	| Knight
	| Bishop
	| Rook
	| King
	| Queen

type piece = {
		id: string; (* each piece will be identified by a unique id e.g. P4 or B2 *)
		team: team; (* black or white *)
		name: string; (* piece type *)
		piecetype: piecekind;
	}

type boardpos = string * string (* (row,column) e.g. (5,a) *)
type move = piece * boardpos * boardpos  (* represents a PHYSICAL move --> (piece, src, dest) *)
type square = boardpos * piece option (* (position,piece); piece=None if the suare is empty *)
type row = (string * square ref) list (* (row #, list of squares) *)
type board = (string * row) list

(* standard chess board size *)
let brd_size = 8

let create_piece (id_1:string) (team_1: team) (name_1:string) (piecetype_1: piecekind) : piece =
	{id = id_1; team=team_1 ; name=name_1 ; piecetype = piecetype_1}

(* get nth letter of alphabet, lowercase *)
let get_nth_letter (n:int) =
	String.lowercase (Char.escaped (Char.chr (n+65)))


(*Precondition: s must only have one character*)
let get_int_of_letter (s:string) =
	let c = s.[0] in
	(Char.code c) - 96


(* make a single board row with id "num" and all empty squares *)
let make_empty_row (num:int) : row =
	let numstr = string_of_int num in
	let unitlist = [();();();();();();();()] in
	let make_square (i:int) =
		let column = get_nth_letter i in
		let pos =(numstr,column) in
		let sq = ref (pos,None) in
		(column ,sq)
	in
	List.mapi (fun i ()-> make_square i)  unitlist


let make_empty_board () : board =
	let unitlist = [();();();();();();();()] in
	List.mapi (fun i () -> (string_of_int i,make_empty_row i)) unitlist

(* converts a boardpos into integer x,y board coordinates *)
let boardpos_to_coords pos = 
	let (xstr,ystr) = pos in
	(int_of_string xstr,get_int_of_letter ystr)

(* (row,column) e.g. (5,a) *)
let get_square_on_board (board_pos: boardpos) (the_board: board): square ref =
	let row_index = int_of_string (fst (board_pos)) in
	let column_index = get_int_of_letter (snd board_pos) in
	let row = snd (List.nth the_board row_index) in
	let the_square = List.nth row column_index in
	match the_square with
	| (_,x) -> x

(* puts a piece p into a square sq *)
let fill_square (sq:square) (p:piece) : square =
	match sq with
	| (board_pos, Some x) -> (board_pos, Some p)
	| (board_pos, None) -> (board_pos, Some p)


let add_piece_to_board (board:board) (pos:boardpos) (id) (team) (name) (piecetype): unit =
	let a1 = get_square_on_board pos board in
	let new_piece = create_piece (id) (team) (name) (piecetype) in
	let new_square = fill_square (!a1) (new_piece) in
	a1:=new_square


let make_init_board () =
	let empty_board = make_empty_board () in
	let _ = add_piece_to_board (empty_board) ("1", "a") ("R1") (White) ("Rook") (Rook) in
	let _ = add_piece_to_board (empty_board) ("1", "b") ("K1") (White) ("Knight") (Knight) in
	let _ = add_piece_to_board (empty_board) ("1", "c") ("B1") (White) ("Bishop") (Bishop) in
	let _ = add_piece_to_board (empty_board) ("1", "d") ("Q") (White) ("Queen") (Queen) in
	let _ = add_piece_to_board (empty_board) ("1", "e") ("K") (White) ("King") (King) in
	let _ = add_piece_to_board (empty_board) ("1", "f") ("B2") (White) ("Bishop") (Bishop) in
	let _ = add_piece_to_board (empty_board) ("1", "g") ("K2") (White) ("Knight") (Knight) in
	let _ = add_piece_to_board (empty_board) ("1", "h") ("R2") (White) ("Rook") (Rook) in

	let _ = add_piece_to_board (empty_board) ("2", "a") ("P1") (White) ("Pawn") (Pawn) in
	let _ = add_piece_to_board (empty_board) ("2", "b") ("P2") (White) ("Pawn") (Pawn) in
	let _ = add_piece_to_board (empty_board) ("2", "c") ("P3") (White) ("Pawn") (Pawn) in
	let _ = add_piece_to_board (empty_board) ("2", "d") ("P4") (White) ("Pawn") (Pawn) in
	let _ = add_piece_to_board (empty_board) ("2", "e") ("P5") (White) ("Pawn") (Pawn) in
	let _ = add_piece_to_board (empty_board) ("2", "f") ("P6") (White) ("Pawn") (Pawn) in
	let _ = add_piece_to_board (empty_board) ("2", "g") ("P7") (White) ("Pawn") (Pawn) in
	let _ = add_piece_to_board (empty_board) ("2", "h") ("P8") (White) ("Pawn") (Pawn) in

	let _ = add_piece_to_board (empty_board) ("8", "a") ("R2") (Black) ("Rook") (Rook) in
	let _ = add_piece_to_board (empty_board) ("8", "b") ("K2") (Black) ("Knight") (Knight) in
	let _ = add_piece_to_board (empty_board) ("8", "c") ("B2") (Black) ("Bishop") (Bishop) in
	let _ = add_piece_to_board (empty_board) ("8", "d") ("K") (Black) ("Queen") (King) in
	let _ = add_piece_to_board (empty_board) ("8", "e") ("Q") (Black) ("King") (Queen) in
	let _ = add_piece_to_board (empty_board) ("8", "f") ("B1") (Black) ("Bishop") (Bishop) in
	let _ = add_piece_to_board (empty_board) ("8", "g") ("K1") (Black) ("Knight") (Knight) in
	let _ = add_piece_to_board (empty_board) ("8", "h") ("R1") (Black) ("Rook") (Rook) in

	let _ = add_piece_to_board (empty_board) ("7", "a") ("P8") (Black) ("Pawn") (Pawn) in
	let _ = add_piece_to_board (empty_board) ("7", "b") ("P7") (Black) ("Pawn") (Pawn) in
	let _ = add_piece_to_board (empty_board) ("7", "c") ("P6") (Black) ("Pawn") (Pawn) in
	let _ = add_piece_to_board (empty_board) ("7", "d") ("P5") (Black) ("Pawn") (Pawn) in
	let _ = add_piece_to_board (empty_board) ("7", "e") ("P4") (Black) ("Pawn") (Pawn) in
	let _ = add_piece_to_board (empty_board) ("7", "f") ("P3") (Black) ("Pawn") (Pawn) in
	let _ = add_piece_to_board (empty_board) ("7", "g") ("P2") (Black) ("Pawn") (Pawn) in
	let _ = add_piece_to_board (empty_board) ("7", "h") ("P1") (Black) ("Pawn") (Pawn) in
	empty_board


(*type move = piece * boardpos * boardpos  (* represents a PHYSICAL move --> (piece, src, dest) *)*)

let execute_move (move:move) board =
	match move with
	| (p, src, dest) ->
	let first_square = get_square_on_board src board in
	let second_square = get_square_on_board dest board in
	first_square:= (src, None) ;
	second_square:= (dest, Some p) ;
	Some board

let coords_of_move (m:move) = 
	let (p,src,dest) = m in
	(boardpos_to_coords src,boardpos_to_coords dest)



(* ---  DISPLAY  --- *)

(*Converts a valid chess board into a piecekind list list which can be converted
	to a string list list for use in display*)
let display_helper_1 (board:board) =
	let rec extract_x x =
		match x with
			| [] -> []
			| (a,b)::t -> b::(extract_x t)
	in
	let rec extract_pieces squares =
		match squares with
			| [] -> []
			| h::t -> let (a,b) = !h in b::(extract_pieces t)
	in
	let rec extract_piecekinds pieces =
		match pieces with
			| [] -> []
			| h::t -> match h with
									| None -> None::(extract_piecekinds t)
									| Some h -> (Some h.piecetype)::(extract_piecekinds t)
	in
	let rows = extract_x board in
	let squaress = List.map (extract_x) (rows) in
	let piecess = List.map (extract_pieces) (squaress) in
	List.map (extract_piecekinds) (piecess)

(*Converts a piecekind option list list to a string list list for use in
	display*)
let display_helper_2 piecekindss =
	let rec f piecekinds =
		match piecekinds with
			| [] -> []
			| h::t -> match h with
								| None -> " "::(f t)
								| Some Pawn -> "Pawn"::(f t)
								| Some Knight -> "Knight"::(f t)
								| Some Bishop -> "Bishop"::(f t)
								| Some Rook -> "Rook"::(f t)
								| Some King -> "King"::(f t)
								| Some Queen -> "Queen"::(f t)
	in
	List.map f piecekindss

(*Prints a string representation of a board*)
let display board =
  let piecekindss = display_helper_1 board in
  let strss = display_helper_2 piecekindss in
  let rec f strs =
  	match strs with
  		| [] -> print_endline("")
  		| h::t -> print_string(" "^h^" "); f t
  in
  List.iter f strss
