open Chesstypes
open Chessmodel
open Rules
open Demoboards
open Util
open Game

(*

	Test Special Rules
		> Castling
		> EnPassant
		> PawnPromotion
		  etc.

*)


TEST_MODULE "castling" = struct

	let board_1 = make_empty_board()

	(* white pieces *)
	let _ = add_piece_to_board (board_1) ("1", "e") ("K") (White) ("King") (King)
	let _ = add_piece_to_board (board_1) ("1", "a") ("R1" ) (White) ("Rook") (Rook)
	let _ = add_piece_to_board (board_1) ("1", "h") ("R2" ) (White) ("Rook") (Rook)
	(* black pieces *)
	let _ = add_piece_to_board (board_1) ("8", "d") ("K") (Black) ("King") (King)
	let _ = add_piece_to_board (board_1) ("8", "h") ("R1" ) (Black) ("Rook") (Rook)
	let _ = add_piece_to_board (board_1) ("8", "a") ("R2" ) (Black) ("Rook") (Rook)

	let g = {make_empty_game() with board=board_1}

	(*
		test board

			------- Black ---------
			a  b  c  d  e  f  g  h
		8	R2 -- -- K  -- -- -- R1  <--black
		7	-- -- -- -- -- -- -- --  
		6	-- -- -- -- -- -- -- --
		5	-- -- -- -- -- -- -- --
		4	-- -- -- -- -- -- -- --  
		3	-- -- -- -- -- -- -- --  
		2	-- -- -- -- -- -- -- --  
		1	R1 -- -- -- K  -- -- R2 <--white
			------- White ---------
	*)

	(* -- WHITE Team Castling -- *)
 
  	(* if no pieces moved yet *)
 	TEST = ((valid_move (king_white_1,("1","e"),("1","g")) g)=Valid(CastlingRight))
 	TEST = ((valid_move (king_white_1,("1","e"),("1","c")) g)=Valid(CastlingLeft))
 	TEST = ((valid_move (king_white_1,("1","e"),("1","b")) g)=Invalid(MovementImpossible))

 	(* if one rook moved *)
	let g = {g with moved_pieces=[rook_white_1]}
	TEST = ((valid_move (king_white_1,("1","e"),("1","c")) g)=Invalid(MovementImpossible))
	TEST = ((valid_move (king_white_1,("1","e"),("1","g")) g)=Valid(CastlingRight))

 	(* if both rooks moved *)
	let g = {g with moved_pieces=(rook_white_2::g.moved_pieces)}
	TEST = ((valid_move (king_white_1,("1","e"),("1","c")) g)=Invalid(MovementImpossible))
	TEST = ((valid_move (king_white_1,("1","e"),("1","g")) g)=Invalid(MovementImpossible))

 	(* if king moved *)
 	let g = {make_empty_game() with board=board_1;moved_pieces=[king_white_1]}
	TEST = ((valid_move (king_white_1,("1","e"),("1","c")) g)=Invalid(MovementImpossible))
	TEST = ((valid_move (king_white_1,("1","e"),("1","g")) g)=Invalid(MovementImpossible))
	
	(* -- BLACK Team Castling -- *)

	(* if no pieces moved yet *)
 	TEST = ((valid_move (king_black_1,("8","d"),("8","f")) g)=Valid(CastlingLeft))
 	TEST = ((valid_move (king_black_1,("8","d"),("8","b")) g)=Valid(CastlingRight))

	(* if one rook moved *)
	let g = {g with moved_pieces=[rook_black_1]}
 	TEST = ((valid_move (king_black_1,("8","d"),("8","f")) g)=Invalid(MovementImpossible))
 	TEST = ((valid_move (king_black_1,("8","d"),("8","b")) g)=Valid(CastlingRight))

	(* if both rooks moved *)
	let g = {g with moved_pieces=rook_black_2::g.moved_pieces}
 	TEST = ((valid_move (king_black_1,("8","d"),("8","f")) g)=Invalid(MovementImpossible))
 	TEST = ((valid_move (king_black_1,("8","d"),("8","b")) g)=Invalid(MovementImpossible))

 	(* if king moved *)
 	let g = {make_empty_game() with board=board_1;moved_pieces=[king_black_1]}
	TEST = ((valid_move (king_black_1,("8","d"),("8","f")) g)=Invalid(MovementImpossible))
	TEST = ((valid_move (king_black_1,("8","d"),("8","b")) g)=Invalid(MovementImpossible))


	(*
		test board with castling obstructions

			------- Black ---------
			a  b  c  d  e  f  g  h
		8	R2 -- -- K  -- -- -- R1  <--black
		7	-- -- -- -- -- -- -- --  
		6	-- -- -- -- -- -- -- --
		5	-- -- -- -- -- -- -- --
		4	-- -- -- -- -- -- -- --  
		3	-- -- -- -- -- -- -- --  
		2	-- -- -- -- -- -- -- --  
		1	R1 -- -- -- K  K2 -- R2 <--white
			------- White ---------
	*)

	(* add an obstruction *)
	let _ = add_piece_to_board (board_1) ("1", "f") ("K2" ) (White) ("Knight") (Knight)

	(* White CastlingRight obstructed *)
 	TEST = ((valid_move (king_white_1,("1","e"),("1","g")) g)=Invalid(MovementImpossible))
 	(* White CastlingLeft open for business *)
 	TEST = ((valid_move (king_white_1,("1","e"),("1","c")) g)=Valid(CastlingLeft))


end

TEST_MODULE "en passant" = struct

	let board_1 = make_empty_board()
	let _ = add_piece_to_board (board_1) ("5", "b") ("P2") (White) ("Pawn") (Pawn) 
	let _ = add_piece_to_board (board_1) ("5", "c") ("P6") (Black) ("Pawn") (Pawn)

	let _ = add_piece_to_board (board_1) ("5", "f") ("P1") (White) ("Pawn") (Pawn) 
	let _ = add_piece_to_board (board_1) ("5", "g") ("P3") (Black) ("Pawn") (Pawn) 

	(*
		test board

			------- Black ---------
			a  b  c  d  e  f  g  h
		8	-- -- -- -- -- -- -- --  
		7	-- -- -- -- -- -- -- --  
		6	-- -- -- -- -- -- -- --
		5	-- P2 P6 -- -- P1 P3 --
		4	-- -- -- -- -- -- -- --  
		3	-- -- -- -- -- -- -- --  
		2	-- -- -- -- -- -- -- --  
		1	-- -- -- -- -- -- -- -- 
			------- White ---------
	*)

	let g = {make_empty_game() with board=board_1}
	let g = {g with in_enpassant=Some(pawn_black_6)}

	(* try en passant for white move *)
 	TEST = ((valid_move (pawn_white_2,("5","b"),("6","c")) g)=Valid(EnPassant))
 	TEST = ((valid_move (pawn_white_2,("5","b"),("6","a")) g)=Invalid(MovementImpossible))

	let g = {make_empty_game() with board=board_1}
 	let g = {g with in_enpassant=Some(pawn_white_2)}

 	(* try en passant for black move *)
 	TEST = ((valid_move (pawn_black_6,("5","c"),("4","b")) g)=Valid(EnPassant))
 	TEST = ((valid_move (pawn_black_6,("5","c"),("4","d")) g)=Invalid(MovementImpossible))

 	(* try some more *)
 	let g = {g with in_enpassant=Some(pawn_black_3)}
 	TEST = ((valid_move (pawn_white_1,("5","f"),("6","g")) g)=Valid(EnPassant))
 	TEST = ((valid_move (pawn_white_1,("5","f"),("6","e")) g)=Invalid(MovementImpossible))
 	TEST = ((valid_move (pawn_white_1,("5","f"),("5","g")) g)=Invalid(MovementImpossible))


end
TEST_MODULE "pawn promotion" = struct

	let board_1 = make_empty_board()

	let _ = add_piece_to_board (board_1) ("7", "b") ("P1") (White) ("Pawn") (Pawn) 
	let _ = add_piece_to_board (board_1) ("7", "e") ("P2") (White) ("Pawn") (Pawn) 
	let _ = add_piece_to_board (board_1) ("7", "h") ("P3") (White) ("Pawn") (Pawn) 
	let _ = add_piece_to_board (board_1) ("6", "c") ("P4") (White) ("Pawn") (Pawn) 

	let _ = add_piece_to_board (board_1) ("2", "c") ("P1") (Black) ("Pawn") (Pawn)

	(*
		test board

			------- Black ---------
			a  b  c  d  e  f  g  h
		8	-- -- -- -- -- -- -- --  
		7	-- P1 -- -- P2 -- -- P3 <-- whites 
		6	-- -- P4 -- -- -- -- --
		5	-- -- -- -- -- -- -- --
		4	-- -- -- -- -- -- -- --  
		3	-- -- -- -- -- -- -- --  
		2	-- -- P1 -- -- -- -- -- <-- blacks
		1	-- -- -- -- -- -- -- -- 
			------- White ---------
	*)

	let g = {make_empty_game() with board=board_1}

 
 	TEST = ((valid_move (pawn_white_1,("7","b"),("8","b")) g)=Valid(PawnPromotion))
 	TEST = ((valid_move (pawn_white_2,("7","e"),("8","e")) g)=Valid(PawnPromotion))
 	TEST = ((valid_move (pawn_white_3,("7","h"),("8","h")) g)=Valid(PawnPromotion))
 	TEST = ((valid_move (pawn_white_4,("6","c"),("7","c")) g)=Valid(Basic))

 	TEST = ((valid_move (pawn_black_1,("2","c"),("1","c")) g)=Valid(PawnPromotion))
 	TEST = ((valid_move (pawn_black_1,("2","c"),("3","c")) g)=Invalid(MovementImpossible))

end

Pa_ounit_lib.Runtime.summarize ()

