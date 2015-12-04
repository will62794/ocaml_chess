open Chesstypes
open Chessmodel
open Rules
open Demoboards
open Util
open Game

(* 

	Test rules.ml functions that have to do with finding all movements that are possible for pieces to make 
	(Inverse Rule Calculations)
*)


TEST_MODULE "possible_movements - simple board" = struct

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

	let g = {make_empty_game() with board=board_1}

	(* -- Black Pawn 1 -- *)
	let mvmts = (possible_movements pawn_black_1 g)
	let expected = [
					((pawn_black_1,("7","e"),("6","e")),Basic);
					((pawn_black_1,("7","e"),("5","e")),Basic)
				]
	TEST = (List.length mvmts) = 2
	TEST = List.for_all (fun x -> List.mem x mvmts) expected
		 

	(* -- White Queen -- *)
	let mvmts = (possible_movements queen_white_1 g)
	(* check a subset of the possible moves *)
	let expected = [
					((queen_white_1,("3","a"),("8","a")),Basic);
					((queen_white_1,("3","a"),("1","a")),Basic);
					((queen_white_1,("3","a"),("7","e")),Capture);
					((queen_white_1,("3","a"),("3","d")),Basic)
				]
	TEST = (List.length mvmts) = 7+2+3+4
	TEST = List.for_all (fun x -> List.mem x mvmts) expected


	(* -- Black Rook 1 -- *)
	let mvmts = (possible_movements rook_black_1 g)
	(* check a subset(or maybe all) of the possible moves *)
	let expected = [
					((rook_black_1,("8","c"),("8","a")),Basic);
					((rook_black_1,("8","c"),("8","b")),Basic);
					((rook_black_1,("8","c"),("8","d")),Basic);
					((rook_black_1,("8","c"),("8","e")),Basic);
					((rook_black_1,("8","c"),("7","c")),Basic);
					((rook_black_1,("8","c"),("6","c")),Basic);
					((rook_black_1,("8","c"),("5","c")),Basic);
					((rook_black_1,("8","c"),("4","c")),Capture);

				]
	TEST = (List.length mvmts) = 8
	TEST = List.for_all (fun x -> List.mem x mvmts) expected


	(* White Bishop 1 *)
	let mvmts = (possible_movements bishop_white_1 g)
	TEST = (List.length mvmts)=6

	
end


TEST_MODULE "possible_movements - full board" = struct
	
	(*
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
	*)

	(* --- full_board --- *)
	let board_full = make_init_board()
	let g = {make_empty_game() with board=board_full}
		
	(* Pawns - 2 initial moves *)
	let mvmts = (possible_movements pawn_white_1 g)
	TEST = (List.length mvmts)=2
	let mvmts = (possible_movements pawn_black_1 g)
	TEST = (List.length mvmts)=2
	let mvmts = (possible_movements pawn_white_3 g)
	TEST = (List.length mvmts)=2

	(* Queens - 0 initial moves *)
	let mvmts = (possible_movements queen_white_1 g)
	TEST = (List.length mvmts)=0
	let mvmts = (possible_movements queen_black_1 g)
	TEST = (List.length mvmts)=0

	(* Knights - 0 initial moves *)
	let mvmts = (possible_movements knight_white_1 g)
	TEST = (List.length mvmts)=2 
	let mvmts = (possible_movements knight_white_2 g)
	TEST = (List.length mvmts)=2

	(* Bishops - 0 initial moves *)
	let mvmts = (possible_movements bishop_white_1 g)
	TEST = (List.length mvmts)=0

end



TEST_MODULE "pieces_capturable_by_moves - simple" = struct

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

	let g = {make_empty_game() with board=board_1}

	(* -- White Queen as Attacker -- *)
	let mvmts = (possible_movements queen_white_1 g)
	let vulnerable_pcs = pieces_capturable_by_moves mvmts board_1
	let expected_pcs = [pawn_black_1]

	TEST = (List.length vulnerable_pcs) = 1
	TEST = List.for_all (fun x -> List.mem x vulnerable_pcs) expected_pcs

	(* -- White Rook 2 as Attacker -- *)
	let mvmts = (possible_movements rook_white_2 g)
	let vulnerable_pcs = pieces_capturable_by_moves mvmts board_1
	let expected_pcs = [pawn_black_1]

	TEST = (List.length vulnerable_pcs) = 1
	TEST = List.for_all (fun x -> List.mem x vulnerable_pcs) expected_pcs

(* 	let mvmts = possible_movements rook_white_1 g
	let vulnerable_pcs = pieces_capturable_by_moves mvmts board_1
	TEST = (List.length vulnerable_pcs)=1 *)

end

TEST_MODULE "pieces_capturable_by_moves - complex" = struct

	let board_1 = make_empty_board()

	(* black pieces *)
	let _ = add_piece_to_board (board_1) ("8", "c") ("R1") (Black) ("Rook") (Rook)
	let _ = add_piece_to_board (board_1) ("5", "g") ("R2") (Black) ("Rook") (Rook)
	let _ = add_piece_to_board (board_1) ("7", "e") ("P1") (Black) ("Pawn") (Pawn)
	let _ = add_piece_to_board (board_1) ("7", "a") ("P2") (Black) ("Pawn") (Pawn)
	let _ = add_piece_to_board (board_1) ("8", "f") ("K1") (Black) ("Knight") (Knight)
	let _ = add_piece_to_board (board_1) ("5", "d") ("K2") (Black) ("Knight") (Knight)

	(* white pieces *)
	let _ = add_piece_to_board (board_1) ("4", "f") ("B1") (White) ("Bishop") (Bishop)
	let _ = add_piece_to_board (board_1) ("3", "h") ("B2") (White) ("Bishop") (Bishop)
	let _ = add_piece_to_board (board_1) ("3", "a") ("Q" ) (White) ("Queen") (Queen)
	let _ = add_piece_to_board (board_1) ("2", "d") ("R1") (White) ("Rook") (Rook)
	let _ = add_piece_to_board (board_1) ("3", "e") ("R2") (White) ("Rook") (Rook)
	let _ = add_piece_to_board (board_1) ("4", "c") ("P1") (White) ("Pawn") (Pawn)
	let _ = add_piece_to_board (board_1) ("3", "g") ("P4") (White) ("Pawn") (Pawn)


	(*
		test board

			------- Black ---------
			a  b  c  d  e  f  g  h
		8	-- -- R1 -- -- K1 -- --  <-- all black pieces
		7	P2 -- -- -- P1 -- -- --  <-- all black pieces
		6	-- -- -- -- -- -- -- --
		5	-- -- -- K2 -- -- R2 --  <-- all black pieces
		4	-- -- P1 -- -- B1 -- --  <-- all white pieces
		3	Q1 -- -- -- R2 -- P4 B2  <-- all white pieces
		2	-- -- -- R1 -- -- -- --  <-- all white pieces
		1	-- -- -- -- -- -- -- --
			------- White ---------
	*)

	let g = {make_empty_game() with board=board_1}

	(* -- Black Knight 2 as Attacker -- *)
	let mvmts = (possible_movements knight_black_2 g)
	let vulnerable_pcs = pieces_capturable_by_moves mvmts board_1
	let expected_pcs = [bishop_white_1;rook_white_2]

	TEST = (List.length vulnerable_pcs) = 2
	TEST = List.for_all (fun x -> List.mem x vulnerable_pcs) expected_pcs

	(* -- White Bishop 2 as Attacker -- *)
	let mvmts = (possible_movements bishop_white_2 g)
	let vulnerable_pcs = pieces_capturable_by_moves mvmts board_1
	let expected_pcs = [rook_black_1]

	TEST = (List.length vulnerable_pcs) = 1
	TEST = List.for_all (fun x -> List.mem x vulnerable_pcs) expected_pcs


	(* -- White Queen as Attacker -- *)
	let mvmts = (possible_movements queen_white_1 g)
	let vulnerable_pcs = pieces_capturable_by_moves mvmts board_1
	let expected_pcs = [pawn_black_2;pawn_black_1]

	TEST = (List.length vulnerable_pcs) = 2
	TEST = List.for_all (fun x -> List.mem x vulnerable_pcs) expected_pcs
end


Pa_ounit_lib.Runtime.summarize ()

