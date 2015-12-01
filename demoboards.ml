open Chesstypes
open Chessmodel

(* Provides some stock board setups suitable for testing of various modules *)



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

let _ = add_piece_to_board (brdsimple1) ("5", "c") ("P1") (Black) ("Pawn") (Pawn) 
let _ = add_piece_to_board (brdsimple1) ("7", "c") ("B1") (White) ("Bishop") (Bishop)
let _ = add_piece_to_board (brdsimple1) ("5", "a") ("Q1") (White) ("Queen") (Queen)
let _ = add_piece_to_board (brdsimple1) ("1", "c") ("R1") (White) ("Rook") (Rook)

let demo_board_simple_1 () =  brdsimple1