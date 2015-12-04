open Chesstypes
open Chessmodel
open Rules
open Demoboards
open Util
open Game


TEST_MODULE "basic piece movements - single piece" = struct

	(* --- PAWNS --- *)
	let g = make_empty_game()
	let _ = add_piece_to_board (g.board) ("2", "a") ("P1") (White) ("Pawn") (Pawn) 
	(* white *)
	TEST = ((valid_move (pawn_white_1,("2","a"),("3","a")) g)=Valid(Basic))
	TEST = ((valid_move (pawn_white_1,("2","a"),("4","a")) g)=Valid(Basic))
	(* invalids *)
	TEST = ((valid_move (pawn_white_1,("2","a"),("5","a")) g)=Invalid(MovementImpossible))
	TEST = ((valid_move (pawn_white_1,("2","a"),("3","b")) g)=Invalid(MovementImpossible))
	TEST = ((valid_move (pawn_white_1,("3","b"),("7","b")) g)=Invalid(MovementImpossible))
	TEST = ((valid_move (pawn_white_1,("2","a"),("6","b")) g)=Invalid(MovementImpossible))
	let g = make_empty_game()
	let _ = add_piece_to_board (g.board) ("7", "c") ("P1") (Black) ("Pawn") (Pawn) 
	(* black *)
	TEST = ((valid_move (pawn_black_3,("7","c"),("6","c")) g)=Valid(Basic))
	TEST = ((valid_move (pawn_black_3,("7","c"),("5","c")) g)=Valid(Basic))
	TEST = ((valid_move (pawn_black_3,("7","c"),("8","c")) g)=Invalid(MovementImpossible))
	TEST = ((valid_move (pawn_black_3,("7","c"),("9","c")) g)=Invalid(MovementImpossible))
	TEST = ((valid_move (pawn_black_3,("7","c"),("3","c")) g)=Invalid(MovementImpossible))


	(* --- KNIGHTS --- *)
	let g = make_empty_game()
	let _ = add_piece_to_board (g.board) ("5", "a") ("K1") (White) ("Knight") (Knight) 
	(* white *)
	TEST = ((valid_move (knight_white_1,("5","a"),("6","c")) g)=Valid(Basic)) (* (1,2) *)
	TEST = ((valid_move (knight_white_1,("5","c"),("6","a")) g)=Valid(Basic)) (* (1,-2) *)
	TEST = ((valid_move (knight_white_1,("5","c"),("4","e")) g)=Valid(Basic)) (* (-1,2) *)
	TEST = ((valid_move (knight_white_1,("5","c"),("4","a")) g)=Valid(Basic)) (* (-1,-2) *)
	TEST = ((valid_move (knight_white_1,("5","a"),("7","b")) g)=Valid(Basic)) (* (2,1) *)
	TEST = ((valid_move (knight_white_1,("5","c"),("7","b")) g)=Valid(Basic)) (* (2,-1) *)
	TEST = ((valid_move (knight_white_1,("5","c"),("3","d")) g)=Valid(Basic)) (* (-2,1) *)
	TEST = ((valid_move (knight_white_1,("5","c"),("3","b")) g)=Valid(Basic)) (* (-2,-1) *)
	(* invalids *)
	TEST = ((valid_move (knight_white_1,("5","a"),("8","b")) g)=Invalid(MovementImpossible))
	TEST = ((valid_move (knight_white_1,("5","a"),("7","d")) g)=Invalid(MovementImpossible))
	let g = make_empty_game()
	let _ = add_piece_to_board (g.board) ("2", "b") ("K1") (Black) ("Knight") (Knight) 
	(* black *)
	TEST = ((valid_move (knight_black_1,("6","c"),("7","e")) g)=Valid(Basic))
	TEST = ((valid_move (knight_black_1,("6","c"),("7","a")) g)=Valid(Basic))
	TEST = ((valid_move (knight_black_1,("6","c"),("7","a")) g)=Valid(Basic))
	TEST = ((valid_move (knight_black_1,("6","c"),("7","a")) g)=Valid(Basic))


	(* --- ROOKS --- *)
	let g = make_empty_game()
	let _ = add_piece_to_board (g.board) ("5", "d") ("R1") (White) ("Rook") (Rook) 
	(* white *)
	(* up/down *)
	TEST = ((valid_move (rook_white_1,("5","d"),("8","d")) g)=Valid(Basic))
	TEST = ((valid_move (rook_white_1,("5","d"),("7","d")) g)=Valid(Basic))
	TEST = ((valid_move (rook_white_1,("5","d"),("6","d")) g)=Valid(Basic))
	TEST = ((valid_move (rook_white_1,("5","d"),("4","d")) g)=Valid(Basic))
	TEST = ((valid_move (rook_white_1,("5","d"),("3","d")) g)=Valid(Basic))
	(* left/right *)
	TEST = ((valid_move (rook_white_1,("5","d"),("5","g")) g)=Valid(Basic))
	TEST = ((valid_move (rook_white_1,("5","d"),("5","h")) g)=Valid(Basic))
	TEST = ((valid_move (rook_white_1,("5","d"),("5","f")) g)=Valid(Basic))
	TEST = ((valid_move (rook_white_1,("5","d"),("5","e")) g)=Valid(Basic))
	TEST = ((valid_move (rook_white_1,("5","d"),("5","c")) g)=Valid(Basic))
	TEST = ((valid_move (rook_white_1,("5","d"),("5","b")) g)=Valid(Basic))
	TEST = ((valid_move (rook_white_1,("5","d"),("5","a")) g)=Valid(Basic))
	(* invalids *)
	TEST = ((valid_move (rook_white_1,("5","d"),("6","h")) g)=Invalid(MovementImpossible))
	TEST = ((valid_move (rook_white_1,("5","d"),("7","h")) g)=Invalid(MovementImpossible))
	TEST = ((valid_move (rook_white_1,("5","d"),("5","d")) g)=Invalid(MovementImpossible))


	(* --- BISHOPS --- *)
	let g = make_empty_game()
	let _ = add_piece_to_board (g.board) ("3", "d") ("B1") (White) ("Bishop") (Bishop)
	(* white *)
	(* southwest *)
	TEST = ((valid_move (bishop_white_1,("3","d"),("2","c")) g)=Valid(Basic))
	TEST = ((valid_move (bishop_white_1,("3","d"),("1","b")) g)=Valid(Basic))
	(* southeast *)
	TEST = ((valid_move (bishop_white_1,("3","d"),("4","c")) g)=Valid(Basic))
	TEST = ((valid_move (bishop_white_1,("3","d"),("5","b")) g)=Valid(Basic))
	TEST = ((valid_move (bishop_white_1,("3","d"),("6","a")) g)=Valid(Basic))
	(* northeast *)
	TEST = ((valid_move (bishop_white_1,("3","d"),("4","e")) g)=Valid(Basic))
	TEST = ((valid_move (bishop_white_1,("3","d"),("5","f")) g)=Valid(Basic))
	TEST = ((valid_move (bishop_white_1,("3","d"),("6","g")) g)=Valid(Basic))
	TEST = ((valid_move (bishop_white_1,("3","d"),("7","h")) g)=Valid(Basic))
	(* northwest *)
	TEST = ((valid_move (bishop_white_1,("3","d"),("2","e")) g)=Valid(Basic))
	TEST = ((valid_move (bishop_white_1,("3","d"),("1","f")) g)=Valid(Basic))
	TEST = ((valid_move (bishop_white_1,("3","d"),("1","f")) g)=Valid(Basic))
	

	(* --- QUEENS --- *)
	let g = make_empty_game()
	let _ = add_piece_to_board (g.board) ("3", "d") ("Q") (White) ("Queen") (Queen)

	(* orthogonal *)
	TEST = ((valid_move (queen_white_1,("3","d"),("8","d")) g)=Valid(Basic))
	TEST = ((valid_move (queen_white_1,("3","d"),("1","d")) g)=Valid(Basic))
	TEST = ((valid_move (queen_white_1,("3","d"),("3","b")) g)=Valid(Basic))
	TEST = ((valid_move (queen_white_1,("3","d"),("3","g")) g)=Valid(Basic))
	(* diagonal *)
	TEST = ((valid_move (queen_white_1,("5","d"),("6","e")) g)=Valid(Basic))
	TEST = ((valid_move (queen_white_1,("5","d"),("6","c")) g)=Valid(Basic))
	TEST = ((valid_move (queen_white_1,("5","d"),("4","e")) g)=Valid(Basic))
	TEST = ((valid_move (queen_white_1,("5","d"),("4","c")) g)=Valid(Basic))


	(* --- KINGS --- *)
	let g = make_empty_game()
	let _ = add_piece_to_board (g.board) ("3", "d") ("K") (White) ("King") (King)

	TEST = ((valid_move (king_white_1,("3","d"),("3","e")) g)=Valid(Basic))
	TEST = ((valid_move (king_white_1,("3","d"),("4","e")) g)=Valid(Basic))
	TEST = ((valid_move (king_white_1,("3","d"),("4","d")) g)=Valid(Basic))
	TEST = ((valid_move (king_white_1,("3","d"),("4","c")) g)=Valid(Basic))
	TEST = ((valid_move (king_white_1,("3","d"),("3","c")) g)=Valid(Basic))
	TEST = ((valid_move (king_white_1,("3","d"),("2","c")) g)=Valid(Basic))
	TEST = ((valid_move (king_white_1,("3","d"),("2","d")) g)=Valid(Basic))
	TEST = ((valid_move (king_white_1,("3","d"),("4","c")) g)=Valid(Basic))
	TEST = ((valid_move (king_white_1,("3","d"),("8","d")) g)=Invalid(MovementImpossible))
	TEST = ((valid_move (king_white_1,("3","d"),("8","d")) g)=Invalid(MovementImpossible))
	TEST = ((valid_move (king_white_1,("3","d"),("5","d")) g)=Invalid(MovementImpossible))
	TEST = ((valid_move (king_white_1,("3","d"),("5","f")) g)=Invalid(MovementImpossible))

end


TEST_MODULE "basic piece movements - multiple pieces" = struct
	
	let board_1 = make_empty_board()

	(* black pieces *)
	let _ = add_piece_to_board (board_1) ("8", "c") ("R1") (Black) ("Rook") (Rook)
	let _ = add_piece_to_board (board_1) ("7", "e") ("P1") (Black) ("Pawn") (Pawn)
	let _ = add_piece_to_board (board_1) ("8", "f") ("K1") (Black) ("Knight") (Knight)

	(* white pieces *)
	let _ = add_piece_to_board (board_1) ("4", "f") ("B1") (White) ("Bishop") (Bishop)
	let _ = add_piece_to_board (board_1) ("3", "a") ("Q" ) (White) ("Queen") (Queen)
	let _ = add_piece_to_board (board_1) ("2", "d") ("R1") (White) ("Rook") (Rook)
	let _ = add_piece_to_board (board_1) ("3", "e") ("R2") (White) ("Rook") (Rook)
	let _ = add_piece_to_board (board_1) ("4", "c") ("P1") (White) ("Pawn") (Pawn)
	let _ = add_piece_to_board (board_1) ("3", "g") ("P4") (White) ("Pawn") (Pawn)

	let g = {make_empty_game() with board=board_1}

	(*
		test board

			------- Black ---------
			a  b  c  d  e  f  g  h
		8	-- -- R1 -- -- K1 -- --  <-- all black pieces
		7	-- -- -- -- P1 -- -- --  <-- all black pieces
		6	-- -- -- -- -- -- -- --
		5	-- -- -- -- -- -- -- --
		4	-- -- P1 -- -- B1 -- --  <-- all white pieces
		3	Q1 -- -- -- R2 -- P4 --  <-- all white pieces
		2	-- -- -- R1 -- -- -- --  <-- all white pieces
		1	-- -- -- -- -- -- -- --
			------- White ---------
	*)

	(* -- White Queen --  *)
	(* unblocked *)
	TEST = ((valid_move (queen_white_1,("3","a"),("5","a")) g)=Valid(Basic))
	TEST = ((valid_move (queen_white_1,("3","a"),("6","a")) g)=Valid(Basic))
	TEST = ((valid_move (queen_white_1,("3","a"),("7","a")) g)=Valid(Basic))
	TEST = ((valid_move (queen_white_1,("3","a"),("3","d")) g)=Valid(Basic))
	TEST = ((valid_move (queen_white_1,("3","a"),("3","d")) g)=Valid(Basic))
	(* blocked *)
	TEST = ((valid_move (queen_white_1,("3","a"),("3","e")) g)=Invalid(MovementImpossible))
	TEST = ((valid_move (queen_white_1,("3","a"),("8","f")) g)=Invalid(MovementImpossible))
	TEST = ((valid_move (queen_white_1,("3","a"),("3","g")) g)=Invalid(MovementImpossible))


	(* -- White Rook 1 -- *)
	(* unblocked *)
	TEST = ((valid_move (rook_white_1,("2","d"),("3","d")) g)=Valid(Basic))
	TEST = ((valid_move (rook_white_1,("2","d"),("8","d")) g)=Valid(Basic))
	TEST = ((valid_move (rook_white_1,("2","d"),("2","h")) g)=Valid(Basic))

	(* -- White Rook 2 -- *)
	(* unblocked *)
	TEST = ((valid_move (rook_white_2,("3","e"),("5","e")) g)=Valid(Basic))
	TEST = ((valid_move (rook_white_2,("3","e"),("6","e")) g)=Valid(Basic))
	TEST = ((valid_move (rook_white_2,("3","e"),("1","e")) g)=Valid(Basic))
 	(* blocked *)
	TEST = ((valid_move (rook_white_2,("3","e"),("8","e")) g)=Invalid(MovementImpossible))
	TEST = ((valid_move (rook_white_2,("3","e"),("3","g")) g)=Invalid(MovementImpossible))
	TEST = ((valid_move (rook_white_2,("3","e"),("3","h")) g)=Invalid(MovementImpossible))
	TEST = ((valid_move (rook_white_2,("3","e"),("3","a")) g)=Invalid(MovementImpossible))

	(* -- Black Knight 1 -- *)
	(* unblocked *)
	TEST = ((valid_move (knight_black_1,("8","f"),("6","e")) g)=Valid(Basic))
	TEST = ((valid_move (knight_black_1,("8","f"),("6","g")) g)=Valid(Basic))
	TEST = ((valid_move (knight_black_1,("8","f"),("6","f")) g)=Invalid(MovementImpossible))
	TEST = ((valid_move (knight_black_1,("8","f"),("8","e")) g)=Invalid(MovementImpossible))

end

TEST_MODULE "capture movements" = struct
	let board_1 = make_empty_board()

	(* black pieces *)
	let _ = add_piece_to_board (board_1) ("8", "c") ("R1") (Black) ("Rook") (Rook)
	let _ = add_piece_to_board (board_1) ("7", "e") ("P1") (Black) ("Pawn") (Pawn)
	let _ = add_piece_to_board (board_1) ("8", "f") ("K1") (Black) ("Knight") (Knight)

	(* white pieces *)
	let _ = add_piece_to_board (board_1) ("4", "f") ("B1") (White) ("Bishop") (Bishop)
	let _ = add_piece_to_board (board_1) ("3", "a") ("Q" ) (White) ("Queen") (Queen)
	let _ = add_piece_to_board (board_1) ("2", "d") ("R1") (White) ("Rook") (Rook)
	let _ = add_piece_to_board (board_1) ("3", "e") ("R2") (White) ("Rook") (Rook)
	let _ = add_piece_to_board (board_1) ("4", "c") ("P1") (White) ("Pawn") (Pawn)
	let _ = add_piece_to_board (board_1) ("3", "g") ("P4") (White) ("Pawn") (Pawn)

	let g = {make_empty_game() with board=board_1}

	(*
		test board

			------- Black ---------
			a  b  c  d  e  f  g  h
		8	-- -- R1 -- -- K1 -- --  <-- all black pieces
		7	-- -- -- -- P1 -- -- --  <-- all black pieces
		6	-- -- -- -- -- -- -- --
		5	-- -- -- -- -- -- -- --
		4	-- -- P1 -- -- B1 -- --  <-- all white pieces
		3	Q1 -- -- -- R2 -- P4 --  <-- all white pieces
		2	-- -- -- R1 -- -- -- --  <-- all white pieces
		1	-- -- -- -- -- -- -- --
			------- White ---------
	*)

	(* -- White Queen --  *)
	(* capture Pawn 1 Black *)
	TEST = ((valid_move (queen_white_1,("3","a"),("7","e")) g)=Valid(Capture))
	
	(* -- Black Rook 1 --  *)
	(* capture Pawn 1 White *)
	TEST = ((valid_move (rook_black_1,("8","c"),("4","c")) g)=Valid(Capture))

	(* -- White Rook 2 --  *)
	(* capture Pawn 1 Black *)
	TEST = ((valid_move (rook_white_2,("3","e"),("7","e")) g)=Valid(Capture))


end

Pa_ounit_lib.Runtime.summarize ()
