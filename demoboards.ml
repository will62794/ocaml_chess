open Chesstypes
open Chessmodel

(* 
	Provide stock board setups suitable for testing of various modules 

	-- DO NOT MODIFY THESE BOARDS-- 
	they are used in testing routines. If you want a custom board, create  
	a new one, or use an existing one.
*)

(*
	demo_board_simple_1


		------- Black ---------
		a  b  c  d  e  f  g  h
	8	-- -- -- -- -- -- -- --
	7	-- -- B1 -- -- -- -- --
	6	-- -- -- -- -- -- -- --
	5	Q1 -- P1 -- -- -- -- --
	4	-- -- -- -- -- -- -- --
	3	-- -- -- -- -- -- -- --
	2	-- -- -- -- -- -- -- --
	1	-- -- R1 -- -- -- -- --
		------- White ---------
*)

let brdsimple1 = make_empty_board()

(* black pieces *)
let _ = add_piece_to_board (brdsimple1) ("5", "c") ("P1") (Black) ("Pawn") (Pawn)

(* white pieces *)
let _ = add_piece_to_board (brdsimple1) ("7", "c") ("B1") (White) ("Bishop") (Bishop)
let _ = add_piece_to_board (brdsimple1) ("5", "a") ("Q") (White) ("Queen") (Queen)
let _ = add_piece_to_board (brdsimple1) ("1", "c") ("R1") (White) ("Rook") (Rook)

let demo_board_simple_1 () =  brdsimple1


(*
	demo_board_dense_1


		------- Black ---------
		a  b  c  d  e  f  g  h
	8	-- -- R1 -- -- K1 -- --  <-- all black pieces
	7	-- -- B1 -- P1 -- -- --  <-- all black pieces
	6	-- -- -- -- -- -- -- --
	5	-- -- -- -- -- -- -- --
	4	-- -- P1 -- -- -- -- --  <-- all white pieces
	3	Q1 -- -- -- R2 -- P4 --  <-- all white pieces
	2	-- -- -- R1 -- -- -- --  <-- all white pieces
	1	-- -- -- -- -- -- -- --
		------- White ---------
*)

let brddense1 = make_empty_board()

(* black pieces *)
let _ = add_piece_to_board (brddense1) ("8", "c") ("R1") (Black) ("Rook") (Rook)
let _ = add_piece_to_board (brddense1) ("7", "e") ("P1") (Black) ("Pawn") (Pawn)
let _ = add_piece_to_board (brddense1) ("8", "f") ("K1") (Black) ("Knight") (Knight)

(* white pieces *)
let _ = add_piece_to_board (brddense1) ("7", "c") ("B1") (White) ("Bishop") (Bishop)
let _ = add_piece_to_board (brddense1) ("3", "a") ("Q") (White) ("Queen") (Queen)
let _ = add_piece_to_board (brddense1) ("2", "d") ("R1") (White) ("Rook") (Rook)
let _ = add_piece_to_board (brddense1) ("3", "e") ("R2") (White) ("Rook") (Rook)
let _ = add_piece_to_board (brddense1) ("4", "c") ("P1") (White) ("Pawn") (Pawn)
let _ = add_piece_to_board (brddense1) ("3", "g") ("P4") (White) ("Pawn") (Pawn)

let demo_board_dense_1 () =  brddense1
