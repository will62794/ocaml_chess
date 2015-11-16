
(* 

	Chess Board Square Layout

	a8 b8 c8 d8 e8 f8 g8 h8
	a7 b7 c7 d7 e7 f7 g7 h7
	a6 b6 c6 d6 e6 f6 g6 h6
	a5 b5 c5 d5 e5 f5 g5 h5
	a4 b4 c4 d4 e4 f4 g4 h4
	a3 b3 c3 d3 e3 f3 g3 h3
	a2 b2 c2 d2 e2 f2 g2 h2
	a1 b1 c1 d1 e1 f1 g1 h1 

Addressing a square: 

	(row,column)
	e.g. ("5","a")

 	board is an association list that maps (row_index:string -> row:row)
	row is an association lists that maps (col_index:string -> square ref)
*)



type piece = {
		id: string; (* each piece will be identified by a unique id *)
		name: string; (* piece type *)
		team: string; (* black or white *)
	} 

type boardpos = string*string (* (row,column) e.g. (5,a) *)
type move = string * string (* descriptive chess notation (?), or algebraic chess notation? *)
type square = boardpos * piece option (* (column letter, piece) *)
type row = (string * square ref) list (* (row #, list of squares) *)
type board = (string * row) list 

let brd_size = 8

(* get nth letter of alphabet, lowercase *)
let get_nth_letter (n:int) = 
	String.lowercase (Char.escaped (Char.chr (n+65)))

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
	will ultimately depend on format we decide to use for encoding moves
*)
let get_move_dest (m:move) : square = 
	failwith "get_move_dest unimplemented"
