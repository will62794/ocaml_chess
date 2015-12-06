open Chesstypes
open Chessmodel
open Rules
open Demoboards
open Util
open Game

(*

	Test Checks and Checkmates

*)


TEST_MODULE "king in check" = struct

	let board_1 = make_empty_board()

	(* white pieces *)
	let _ = add_piece_to_board (board_1) ("1", "e") ("K") (White) ("King") (King)
	let _ = add_piece_to_board (board_1) ("1", "a") ("R1" ) (White) ("Rook") (Rook)
	let _ = add_piece_to_board (board_1) ("1", "h") ("R2" ) (White) ("Rook") (Rook)
	(* black pieces *)
	let _ = add_piece_to_board (board_1) ("8", "d") ("K") (Black) ("King") (King)
	let _ = add_piece_to_board (board_1) ("8", "h") ("R1" ) (Black) ("Rook") (Rook)
	let _ = add_piece_to_board (board_1) ("8", "a") ("R2" ) (Black) ("Rook") (Rook)
	let _ = add_piece_to_board (board_1) ("3", "e") ("Q" ) (Black) ("Queen") (Queen)


	(*
		test board

			------- Black ---------
			a  b  c  d  e  f  g  h
		8	R2 -- -- K  -- -- -- R1  <--black
		7	-- -- -- -- -- -- -- --  
		6	-- -- -- -- -- -- -- --
		5	-- -- -- -- -- -- -- --
		4	-- -- -- -- -- -- -- --  
		3	-- -- -- -- Q  -- -- --  <--black
		2	-- -- -- -- -- -- -- --  
		1	R1 -- -- -- K  -- -- R2 <--white
			------- White ---------
	*)

	let g = {make_empty_game() with board=board_1}

	TEST = (king_in_check White g)=Some(Check)

	(* force a checkmate *)
	let _ = add_piece_to_board (board_1) ("3", "h") ("B1" ) (Black) ("Bishop") (Bishop)
	let _ = add_piece_to_board (board_1) ("3", "b") ("B2" ) (Black) ("Bishop") (Bishop)

	TEST = (king_in_check White g)=Some(Checkmate)

	let king_move = (king_white_1,("1", "e"),("2", "f"))
	let validation = valid_move king_move g
	TEST = (validation = Invalid(WouldBeCheck))

end

Pa_ounit_lib.Runtime.summarize ()
