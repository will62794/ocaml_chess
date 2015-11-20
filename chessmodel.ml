
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

(* get nth letter of alphabet, lowercase *)
let get_nth_letter (n:int) = 
	String.lowercase (Char.escaped (Char.chr (n+65)))

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
	

let make_empty_board () = 
	let unitlist = [();();();();();();();()] in
	List.mapi (fun i () -> (string_of_int i,make_empty_row i)) unitlist 

let make_init_board () = 
	failwith "make_init_board not implemented"

let execute_move move board = 
	failwith "execute move not implemented"


(* --- HELPERS --- *)

(* puts a piece p into a square sq *)
let fill_square (sq:square) (p:piece) : square = 
	failwith "fill_square unimplemented"

(* 
	returns square of move destination 
	-> implementation note (Will S.): will ultimately depend on format we decide to use for encoding moves
*)
let get_move_dest (m:move) : square = 
	failwith "get_move_dest unimplemented"
