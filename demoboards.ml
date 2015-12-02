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
let _ = add_piece_to_board (brdsimple1) ("5", "a") ("Q1") (White) ("Queen") (Queen)
let _ = add_piece_to_board (brdsimple1) ("1", "c") ("R1") (White) ("Rook") (Rook)

let demo_board_simple_1 () =  brdsimple1