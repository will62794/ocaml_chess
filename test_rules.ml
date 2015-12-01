open Chesstypes
open Chessmodel
open Rules
open Demoboards
open Util

let print_mvmts mvmts = 
	let _ = List.iter (fun (p,s,d) -> print_boardpos d) mvmts in
	print_endline ""

TEST_MODULE "valid_move basic piece moves" = struct
	(* Basic Pawn Moves *)
	let brd = make_empty_board()
	let pawn_white = { id="P1"; team=White; name="Pawn"; piecetype=Pawn; }
	let _ = add_piece_to_board (brd) ("2", "a") ("P1") (White) ("Pawn") (Pawn) 

	TEST = ((valid_move (pawn_white,("2","a"),("3","a")) brd)=true)
	TEST = ((valid_move (pawn_white,("2","a"),("3","b")) brd)=false)
	TEST = ((valid_move (pawn_white,("2","a"),("6","b")) brd)=false)

	let pawn_black = { id="P3"; team=Black; name="Pawn"; piecetype=Pawn; }
	let _ = add_piece_to_board (brd) ("7", "c") ("P1") (Black) ("Pawn") (Pawn) 

	TEST = ((valid_move (pawn_black,("7","c"),("6","c")) brd)=true)
	TEST = ((valid_move (pawn_black,("7","c"),("8","c")) brd)=false)
	TEST = ((valid_move (pawn_black,("7","c"),("9","c")) brd)=false)

	(* Basic Knight Moves *)
	let brd = make_empty_board()
	let knight_white = { id="K1"; team=White; name="Knight"; piecetype=Knight; }
	let _ = add_piece_to_board (brd) ("5", "a") ("K1") (White) ("Knight") (Knight) 
	
	TEST = ((valid_move (knight_white,("5","a"),("6","c")) brd)=true)
	TEST = ((valid_move (knight_white,("5","a"),("7","b")) brd)=true)

	(* Basic Rook Moves *)
	let brd = make_empty_board()
	let rook_white = { id="R1"; team=White; name="Rook"; piecetype=Rook; }
	let _ = add_piece_to_board (brd) ("5", "d") ("R1") (White) ("Rook") (Rook) 

	TEST = ((valid_move (rook_white,("5","d"),("8","d")) brd)=true)
	TEST = ((valid_move (rook_white,("5","d"),("5","g")) brd)=true)
	TEST = ((valid_move (rook_white,("5","d"),("5","h")) brd)=true)
	TEST = ((valid_move (rook_white,("5","d"),("6","h")) brd)=false)
	TEST = ((valid_move (rook_white,("5","d"),("7","h")) brd)=false)

	(* Basic Bishop Moves *)
	let brd = make_empty_board()
	let bishop_white = { id="B1"; team=White; name="Bishop"; piecetype=Bishop; }
	let _ = add_piece_to_board (brd) ("3", "d") ("B1") (White) ("Bishop") (Bishop)

	TEST = ((valid_move (bishop_white,("3","d"),("5","f")) brd)=true)
	TEST = ((valid_move (bishop_white,("3","d"),("1","b")) brd)=true)
	TEST = ((valid_move (bishop_white,("3","d"),("6","a")) brd)=true)

	(* Basic Queen Moves *)
	let brd = make_empty_board()
	let queen_white = { id="Q1"; team=White; name="Queen"; piecetype=Queen; }
	let _ = add_piece_to_board (brd) ("3", "d") ("Q1") (White) ("Queen") (Queen)

	TEST = ((valid_move (queen_white,("3","d"),("8","d")) brd)=true)
	TEST = ((valid_move (queen_white,("3","d"),("6","g")) brd)=true)
	TEST = ((valid_move (queen_white,("3","d"),("1","b")) brd)=true)

	(* Basic King Moves *)
	let brd = make_empty_board()
	let king_white = { id="K1"; team=White; name="King"; piecetype=King; }
	let _ = add_piece_to_board (brd) ("3", "d") ("K1") (White) ("King") (King)

	TEST = ((valid_move (king_white,("3","d"),("4","d")) brd)=true)
	TEST = ((valid_move (king_white,("3","d"),("4","e")) brd)=true)
	TEST = ((valid_move (king_white,("3","d"),("8","d")) brd)=false)

end

TEST_MODULE "valid_move with collisions" = struct
	
	let brd = make_empty_board()
	let pawn_white = { id="P1"; team=White; name="Pawn"; piecetype=Pawn; }
	let bishop_white = { id="B1"; team=White; name="Bishop"; piecetype=Bishop; }
	let queen_white = { id="Q1"; team=White; name="Queen"; piecetype=Queen; }
	let rook_white = { id="R1"; team=White; name="Rook"; piecetype=Rook; }

	let _ = add_piece_to_board (brd) ("5", "c") ("P1") (White) ("Pawn") (Pawn) 
	let _ = add_piece_to_board (brd) ("7", "c") ("B1") (White) ("Bishop") (Bishop)
	let _ = add_piece_to_board (brd) ("5", "a") ("Q1") (White) ("Queen") (Queen)
	let _ = add_piece_to_board (brd) ("1", "c") ("R1") (White) ("Rook") (Rook)

(*
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

	(* queen should be blocked by the pawn *)
	TEST = ((valid_move (queen_white,("5","a"),("5","f")) brd)=false)
	(* queen should be unblocked *)
	TEST = ((valid_move (queen_white,("5","a"),("5","b")) brd)=true)
	(* queen should be blocked by the bishop *)
	TEST = ((valid_move (queen_white,("5","a"),("7","d")) brd)=false)
	(* rook should be blocked by the pawn, and/or bishop *)
	TEST = ((valid_move (rook_white,("1","c"),("6","c")) brd)=false)
	TEST = ((valid_move (rook_white,("1","c"),("8","c")) brd)=false)
end

TEST_MODULE "possible_movements" = struct


	let pawn_white = { id="P1"; team=White; name="Pawn"; piecetype=Pawn; }
	let bishop_white = { id="B1"; team=White; name="Bishop"; piecetype=Bishop; }
	let queen_white = { id="Q1"; team=White; name="Queen"; piecetype=Queen; }
	let rook_white = { id="R1"; team=White; name="Rook"; piecetype=Rook; }

 	(* Pawn *)
	let brd = make_empty_board()
	let _ = add_piece_to_board (brd) ("2", "c") ("P1") (White) ("Pawn") (Pawn) 
	
	let mvmts = possible_movements pawn_white brd
	let _ = List.iter (fun (p,s,d) -> print_boardpos d) mvmts
	TEST = (List.length mvmts)=1
	TEST = (mvmts=[(pawn_white,("2","c"),("3","c"))])

 	(* Bishop *)
	let brd = make_empty_board()
	let _ = add_piece_to_board (brd) ("2", "c") ("B1") (White) ("Bishop") (Bishop) 
	let mvmts = possible_movements bishop_white brd
	let _ = print_mvmts mvmts
	TEST = (List.length mvmts)=9


end

TEST_MODULE "pieces_capturable_by_moves" = struct

	let board1 = demo_board_simple_1()

	let queen = { id="Q1"; team=White; name="Queen"; piecetype=Queen; }
	let mvmts = possible_movements queen board1
	let _ = print_endline "Queen Mvmts:"
	let _ = print_mvmts mvmts
	(* TEST = (List.length mvmts)=15 *)
	let vulnerable_pcs = pieces_capturable_by_moves mvmts board1
	let _ = List.iter print_piece vulnerable_pcs 
(* 	let _ = print_int (List.length vulnerable_pcs)
 *)	TEST = (List.length vulnerable_pcs)=1

end







