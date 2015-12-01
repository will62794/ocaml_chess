open Chesstypes
open Chessmodel
open Rules
open Demoboards
open Util
open Game

let print_mvmts mvmts = 
	let _ = List.iter (fun (p,s,d) -> print_boardpos d) mvmts in
	print_endline ""


TEST_MODULE "valid_move basic piece moves" = struct
	(* Basic Pawn Moves *)
	let g = make_empty_game()
	let pawn_white = { id="P1"; team=White; name="Pawn"; piecetype=Pawn; }
	let _ = add_piece_to_board (g.board) ("2", "a") ("P1") (White) ("Pawn") (Pawn) 


	TEST = ((valid_move (pawn_white,("2","a"),("3","a")) g)=Valid(Basic))
	TEST = ((valid_move (pawn_white,("2","a"),("3","b")) g)=Invalid(MovementImpossible))
	TEST = ((valid_move (pawn_white,("2","a"),("6","b")) g)=Invalid(MovementImpossible))

	let pawn_black = { id="P3"; team=Black; name="Pawn"; piecetype=Pawn; }
	let _ = add_piece_to_board (g.board) ("7", "c") ("P1") (Black) ("Pawn") (Pawn) 

	TEST = ((valid_move (pawn_black,("7","c"),("6","c")) g)=Valid(Basic))
	TEST = ((valid_move (pawn_black,("7","c"),("8","c")) g)=Invalid(MovementImpossible))
	TEST = ((valid_move (pawn_black,("7","c"),("9","c")) g)=Invalid(MovementImpossible))

	(* Basic Knight Moves *)
	let g = make_empty_game()
	let knight_white = { id="K1"; team=White; name="Knight"; piecetype=Knight; }
	let _ = add_piece_to_board (g.board) ("5", "a") ("K1") (White) ("Knight") (Knight) 
	
	TEST = ((valid_move (knight_white,("5","a"),("6","c")) g)=Valid(Basic))
	TEST = ((valid_move (knight_white,("5","a"),("7","b")) g)=Valid(Basic))

	(* Basic Rook Moves *)
	let g = make_empty_game()
	let rook_white = { id="R1"; team=White; name="Rook"; piecetype=Rook; }
	let _ = add_piece_to_board (g.board) ("5", "d") ("R1") (White) ("Rook") (Rook) 

	TEST = ((valid_move (rook_white,("5","d"),("8","d")) g)=Valid(Basic))
	TEST = ((valid_move (rook_white,("5","d"),("5","g")) g)=Valid(Basic))
	TEST = ((valid_move (rook_white,("5","d"),("5","h")) g)=Valid(Basic))
	TEST = ((valid_move (rook_white,("5","d"),("6","h")) g)=Invalid(MovementImpossible))
	TEST = ((valid_move (rook_white,("5","d"),("7","h")) g)=Invalid(MovementImpossible))

	(* Basic Bishop Moves *)
	let g = make_empty_game()
	let bishop_white = { id="B1"; team=White; name="Bishop"; piecetype=Bishop; }
	let _ = add_piece_to_board (g.board) ("3", "d") ("B1") (White) ("Bishop") (Bishop)

	TEST = ((valid_move (bishop_white,("3","d"),("5","f")) g)=Valid(Basic))
	TEST = ((valid_move (bishop_white,("3","d"),("1","b")) g)=Valid(Basic))
	TEST = ((valid_move (bishop_white,("3","d"),("6","a")) g)=Valid(Basic))

	(* Basic Queen Moves *)
	let g = make_empty_game()
	let queen_white = { id="Q1"; team=White; name="Queen"; piecetype=Queen; }
	let _ = add_piece_to_board (g.board) ("3", "d") ("Q1") (White) ("Queen") (Queen)

	TEST = ((valid_move (queen_white,("3","d"),("8","d")) g)=Valid(Basic))
	TEST = ((valid_move (queen_white,("3","d"),("6","g")) g)=Valid(Basic))
	TEST = ((valid_move (queen_white,("3","d"),("1","b")) g)=Valid(Basic))

	(* Basic King Moves *)
	let g = make_empty_game()
	let king_white = { id="K1"; team=White; name="King"; piecetype=King; }
	let _ = add_piece_to_board (g.board) ("3", "d") ("K1") (White) ("King") (King)

	TEST = ((valid_move (king_white,("3","d"),("4","d")) g)=Valid(Basic))
	TEST = ((valid_move (king_white,("3","d"),("4","e")) g)=Valid(Basic))
	TEST = ((valid_move (king_white,("3","d"),("8","d")) g)=Invalid(MovementImpossible))

end

TEST_MODULE "valid_move with collisions" = struct
	
	let g = make_empty_game()
	let pawn_white = { id="P1"; team=White; name="Pawn"; piecetype=Pawn; }
	let bishop_white = { id="B1"; team=White; name="Bishop"; piecetype=Bishop; }
	let queen_white = { id="Q1"; team=White; name="Queen"; piecetype=Queen; }
	let rook_white = { id="R1"; team=White; name="Rook"; piecetype=Rook; }

	let _ = add_piece_to_board (g.board) ("5", "c") ("P1") (White) ("Pawn") (Pawn) 
	let _ = add_piece_to_board (g.board) ("7", "c") ("B1") (White) ("Bishop") (Bishop)
	let _ = add_piece_to_board (g.board) ("5", "a") ("Q1") (White) ("Queen") (Queen)
	let _ = add_piece_to_board (g.board) ("1", "c") ("R1") (White) ("Rook") (Rook)

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
	TEST = ((valid_move (queen_white,("5","a"),("5","f")) g)=Invalid(MovementImpossible))
	(* queen should be unblocked *)
	TEST = ((valid_move (queen_white,("5","a"),("5","b")) g)=Valid(Basic))
	(* queen should be blocked by the bishop *)
	TEST = ((valid_move (queen_white,("5","a"),("7","d")) g)=Invalid(MovementImpossible))
	(* rook should be blocked by the pawn, and/or bishop *)
	TEST = ((valid_move (rook_white,("1","c"),("6","c")) g)=Invalid(MovementImpossible))
	TEST = ((valid_move (rook_white,("1","c"),("8","c")) g)=Invalid(MovementImpossible))
end

TEST_MODULE "possible_movements" = struct


	let pawn_white = { id="P1"; team=White; name="Pawn"; piecetype=Pawn; }
	let bishop_white = { id="B1"; team=White; name="Bishop"; piecetype=Bishop; }
	let queen_white = { id="Q1"; team=White; name="Queen"; piecetype=Queen; }
	let rook_white = { id="R1"; team=White; name="Rook"; piecetype=Rook; }

 	(* Pawn *)
	let g = make_empty_game()
	let _ = add_piece_to_board (g.board) ("2", "c") ("P1") (White) ("Pawn") (Pawn) 
	
	let mvmts = possible_movements pawn_white g
	let _ = List.iter (fun (p,s,d) -> print_boardpos d) mvmts
	TEST = (List.length mvmts)=1
	TEST = (mvmts=[(pawn_white,("2","c"),("3","c"))])

 	(* Bishop *)
	let g = make_empty_game()
	let _ = add_piece_to_board (g.board) ("2", "c") ("B1") (White) ("Bishop") (Bishop) 
	let mvmts = possible_movements bishop_white g
	let _ = print_mvmts mvmts
	TEST = (List.length mvmts)=9


end

TEST_MODULE "pieces_capturable_by_moves" = struct

	let board1 = demo_board_simple_1()
	let g = {make_empty_game() with board=board1}

	let queen = { id="Q1"; team=White; name="Queen"; piecetype=Queen; }
	let mvmts = possible_movements queen g
	let _ = print_endline "Queen Mvmts:"
	let _ = print_mvmts mvmts
	(* TEST = (List.length mvmts)=15 *)
	let vulnerable_pcs = pieces_capturable_by_moves mvmts board1
	let _ = List.iter print_piece vulnerable_pcs 
(* 	let _ = print_int (List.length vulnerable_pcs)
 *)	TEST = (List.length vulnerable_pcs)=1

end







