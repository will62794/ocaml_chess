open Chesstypes

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

  Numerical (for move computations):

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

[ PIECE LABELINGS ]

  ------- Black ---------
  R2 K2 B2 K  Q  B1 K1 R1
  P8 P7 P6 P5 P4 P3 P2 P1
  -- -- -- -- -- -- -- --
  -- -- -- -- -- -- -- --
  -- -- -- -- -- -- -- --
  -- -- -- -- -- -- -- --
  P1 P2 P3 P4 P5 P6 P7 P8
  R1 K1 B1 Q  K  B2 K2 R2
  ------- White ---------

Addressing a square:

  (row,column)
  e.g. ("5","a")

  board is an association list that maps (row_index:string -> row:row)
  row is an association lists that maps (col_index:string -> square ref)

  * each piece also has
  a "team" associated with it i.e. Black | White

*)


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
  List.mapi (fun i () -> (string_of_int (i+1),make_empty_row (i+1))) unitlist

(* converts a boardpos into integer x,y board coordinates *)
let boardpos_to_coords pos =
  let (xstr,ystr) = pos in
  (get_int_of_letter ystr,int_of_string xstr)

let coords_to_boardpos (x,y) =
  ((string_of_int y),(get_nth_letter (x-1)))


(* e.g. "1" -> "8", e.g. "3" -> "5" *)
let invert_row_index row_str =
  let row_int = int_of_string row_str in
  string_of_int((brd_size+1)-row_int)

(* e.g. "a" -> "h", e.g. "b" -> "g" *)
let invert_col_index col_str =
  let col_int = get_int_of_letter col_str in
  get_nth_letter ((brd_size)-col_int)

let invert_boardpos brdpos =
  let row_str,col_str = brdpos in
  (invert_row_index row_str,invert_col_index col_str)

let reflect_board_row row =
  List.map (fun (col,sq) -> ((invert_col_index col),sq) ) row

let reflect_board_coords brd =
  List.map (fun (s,row) -> ((invert_row_index s),(reflect_board_row row))) brd

(* (row,column) e.g. (5,a)

type square = boardpos * piece option (* (position,piece); piece=None if the suare is empty *)
type row = (string * square ref) list (* (row #, list of squares) *)
type board = (string * row) list

*)
let get_square_on_board (board_pos: boardpos) (the_board: board): square ref =
  let row_str,col_str = board_pos in
  let row = List.assoc row_str the_board in
  List.assoc col_str row

(* assumes there is a piece at the square given. will fail if empty *)
let get_piece_at_square (sq:square) =
  let (pos,p) = sq in
  match p with
    | None -> failwith "no piece at square"
    | Some pce -> pce


let get_piece (board:board) (board_pos:boardpos) : piece =
  let square = !(get_square_on_board board_pos board) in
  let piece_option = snd square in
  match piece_option with
  | Some p -> p
  | None -> failwith "square empty"

(* returns a board as a single list of all square refs *)
let flatten_board brd : (square ref) list =
  let _,board_rows = List.split brd in
  let flattened_rows = List.map (fun r -> snd (List.split r)) board_rows in
  List.flatten flattened_rows

(* returns true if the given square is unoccupied by a piece, else false *)
let square_empty (s:square) =
  let (pos,p) = s in
  match pos,p with
    | _,Some x -> false
    | _,None -> true

(* returns true if the given square is unoccupied by a piece, else false *)
let square_filled (s:square) =
  not (square_empty s)

let piece_at_sq (sq:square ref) (pce:piece) =
  let (pos,p) = !sq in
  p = Some(pce)

let find_piece_pos (pce:piece) (brd:board) =
  let flattened_brd = flatten_board brd in
  try
    let found_square = (List.find (fun sq -> (piece_at_sq sq pce)) flattened_brd) in
    Some(fst(!found_square))
  with Not_found -> None

(* returns all pieces, of any team, currently on given board *)
let all_board_pieces (b:board) =
  let flattened_board_sqs = flatten_board b in
  let square_vals = List.map (!) flattened_board_sqs in
  let filled_squares = List.filter square_filled square_vals in
  List.map get_piece_at_square filled_squares

let all_team_pieces (b:board) (t:team) =
  let all_pieces = (all_board_pieces b) in
  List.filter (fun p -> p.team=t) all_pieces

(* puts a piece p into a square sq *)
let fill_square (sq:square) (p:piece) : square =
  let brd_pos = fst sq in
  (brd_pos, Some p)

let add_piece_to_board (board:board) (pos:boardpos) (id) (team) (name) (piecetype): unit =
  let square_ref = get_square_on_board pos board in
  let new_piece = create_piece (id) (team) (name) (piecetype) in
  let new_square = fill_square (!square_ref) (new_piece) in
  square_ref:=new_square

let make_init_board () =
  let empty_board = make_empty_board () in
  let _ = add_piece_to_board (empty_board) ("1", "a") ("R1") (White) ("Rook") (Rook) in
  let _ = add_piece_to_board (empty_board) ("1", "b") ("K1") (White) ("Knight") (Knight) in
  let _ = add_piece_to_board (empty_board) ("1", "c") ("B1") (White) ("Bishop") (Bishop) in
  let _ = add_piece_to_board (empty_board) ("1", "d") ("Q" ) (White) ("Queen") (Queen) in
  let _ = add_piece_to_board (empty_board) ("1", "e") ("K" ) (White) ("King") (King) in
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
  let _ = add_piece_to_board (empty_board) ("8", "e") ("K" ) (Black) ("King") (King) in
  let _ = add_piece_to_board (empty_board) ("8", "d") ("Q" ) (Black) ("Queen") (Queen) in
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

let rec get_pieces_helper (team:team) (thelist: square ref list): piece list =
  match thelist with
  | [] -> []
  | hd::tl->
  let deref_square = (!hd) in
  let piece_opt = snd (deref_square) in
  match piece_opt with
  | None -> (get_pieces_helper team tl)
  | Some p -> if (p.team = team) then (p :: (get_pieces_helper team tl)) else
  (get_pieces_helper team tl )


let get_all_pieces (team:team) (board:board) =
  let flatten_board= flatten_board board in
  get_pieces_helper team flatten_board

let put_piece_at_boardpos (piece:piece option) (board_pos: boardpos) (board:board) : unit=
  let square_ref = get_square_on_board board_pos board in
  square_ref := (board_pos, piece)



(*Deep copies the game by creating new square refs*)
let deep_copy_game (game:game): game =
  game











(* ------------------------ *)
(* ---  DISPLAY  ---------- *)
(* ------------------------ *)


(*** Let's move display code to own module ***)

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


(* ---------------------------*)
(* -------- TESTS ----------- *)
(* ---------------------------*)

let print_boardpos brdpos =
  let (r,c) = brdpos in
  (Printf.printf "(%s,%s)\n" r c)

TEST_MODULE "invert_board_coords" = struct

  let indices = ["a";"b";"c";"d";"e";"f";"g";"h"]
  let expected = ["h";"g";"f";"e";"d";"c";"b";"a"]
  TEST = (List.map invert_col_index indices)=expected

  let indices = ["1";"2";"3";"4";"5";"6";"7";"8"]
  let expected = ["8";"7";"6";"5";"4";"3";"2";"1"]
  TEST = (List.map invert_row_index indices)=expected

  TEST = (invert_boardpos ("2","b"))=("7","g")

end

TEST_MODULE "board utilities" = struct
  TEST = (coords_to_boardpos (3,6))=("6","c")
  TEST = (coords_to_boardpos (1,8))=("8","a")

  let brd = make_empty_board()
  let pawn_white_1 = { id="P1"; team=White; name="Pawn"; piecetype=Pawn; }
  let pawn_white_3 = { id="P3"; team=White; name="Pawn"; piecetype=Pawn; }
  let pawn_white_5 = { id="P5"; team=White; name="Pawn"; piecetype=Pawn; }
  let pawn_white_8 = { id="P8"; team=White; name="Pawn"; piecetype=Pawn; }

  let bishop_white = { id="B1"; team=White; name="Bishop"; piecetype=Bishop; }
  let knight_white_1 = { id="K1"; team=White; name="Knight"; piecetype=Knight; }
  let king_white = { id="K"; team=White; name="King"; piecetype=King; }
  let queen_white = { id="Q"; team=White; name="Queen"; piecetype=Queen; }

  let pawn_black_1 = { id="P1"; team=Black; name="Pawn"; piecetype=Pawn; }
  let pawn_black_3 = { id="P3"; team=Black; name="Pawn"; piecetype=Pawn; }
  let pawn_black_5 = { id="P5"; team=Black; name="Pawn"; piecetype=Pawn; }
  let pawn_black_8 = { id="P8"; team=Black; name="Pawn"; piecetype=Pawn; }
  let queen_black = { id="Q"; team=Black; name="Queen"; piecetype=Queen; }

  let _ = add_piece_to_board (brd) ("2", "c") ("P1") (White) ("Pawn") (Pawn)

  let found_pos = find_piece_pos pawn_white_1 brd
  TEST = (found_pos=Some("2","c"))
  let found_pos = find_piece_pos bishop_white brd
  TEST = (found_pos=None)

  let brd = make_init_board()
  (* white *)
  let found_pos,pos = (find_piece_pos queen_white brd),("1","d")
  TEST = (found_pos=Some(pos))
  let found_pos,pos = (find_piece_pos knight_white_1 brd),("1","b")
  TEST = (found_pos=Some(pos))

  (* black *)
  let found_pos,pos = (find_piece_pos queen_black brd),("8","e")
  TEST = (found_pos=Some(pos))
  let found_pos,pos = (find_piece_pos pawn_black_5 brd),("7","d")
  TEST = (found_pos=Some(pos))
end

TEST_MODULE "board pieces" = struct
  let brd = make_init_board()
  let all_pieces = (all_board_pieces brd)
  TEST = (List.length all_pieces)=32

  let brd = make_init_board()
  let all_white_pieces = (all_team_pieces brd White)
  TEST = (List.length all_white_pieces)=16
  let all_black_pieces = (all_team_pieces brd Black)
  TEST = (List.length all_black_pieces)=16

end




(*

let _ =
let new_board = make_init_board () in
display new_board ;
(*piece * boardpos * boardpos*)
(*boardpos = string * string*)
let board_pos_1 = ("1", "c") in
let board_pos_2 = ("4", "c") in
let piece_1= get_piece new_board board_pos_1 in
let move= (piece_1 , board_pos_1 , board_pos_2) in
let board = match (execute_move move new_board) with
| Some b -> b
| None -> failwith "No piece there"
in
display board


let _ =
let new_board = make_init_board () in
display new_board ;
(*piece * boardpos * boardpos*)
(*boardpos = string * string*)
let board_pos_1 = ("3", "c") in
let board_pos_2 = ("4", "c") in
let piece_1= get_piece new_board board_pos_1 in
let move= (piece_1 , board_pos_1 , board_pos_2) in
let board = match (execute_move move new_board) with
| Some b -> b
| None -> failwith "No board"
in
display board ;

*)

