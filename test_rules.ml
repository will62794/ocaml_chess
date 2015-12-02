open Chesstypes
open Chessmodel
open Rules
open Demoboards
open Util
open Game

let print_mvmts mvmts = 
	let _ = List.iter (fun (p,s,d) -> print_boardpos d) mvmts in
	print_endline ""

(* Test Pieces *)
let pawn_white_1 = { id="P1"; team=White; name="Pawn"; piecetype=Pawn; }
let pawn_white_2 = { id="P2"; team=White; name="Pawn"; piecetype=Pawn; }
let pawn_white_3 = { id="P3"; team=White; name="Pawn"; piecetype=Pawn; }
let pawn_white_4 = { id="P4"; team=White; name="Pawn"; piecetype=Pawn; }

let knight_white_1 = { id="K1"; team=White; name="Knight"; piecetype=Knight; }
let knight_white_2 = { id="K2"; team=White; name="Knight"; piecetype=Knight; }
let bishop_white_1 = { id="B1"; team=White; name="Bishop"; piecetype=Bishop; }
let rook_white_1 = { id="R1"; team=White; name="Rook"; piecetype=Rook; }
let rook_white_2 = { id="R2"; team=White; name="Rook"; piecetype=Rook; }
let queen_white_1 = { id="Q"; team=White; name="Queen"; piecetype=Queen; }
let king_white_1 = { id="K"; team=White; name="King"; piecetype=King; }

let pawn_black_1 = { id="P1"; team=Black; name="Pawn"; piecetype=Pawn; }
let pawn_black_3 = { id="P3"; team=Black; name="Pawn"; piecetype=Pawn; }
let knight_black_1 = { id="K1"; team=Black; name="Knight"; piecetype=Knight; }
let bishop_black_1 = { id="B1"; team=Black; name="Bishop"; piecetype=Bishop; }
let rook_black_1 = { id="R1"; team=Black; name="Rook"; piecetype=Rook; }
let queen_black_1 = { id="Q"; team=Black; name="Queen"; piecetype=Queen; }
let king_black_1 = { id="K"; team=Black; name="King"; piecetype=King; }

TEST_MODULE "valid_move basic piece moves" = struct
	(* Basic Pawn Moves *)
	let g = make_empty_game()
	let _ = add_piece_to_board (g.board) ("2", "a") ("P1") (White) ("Pawn") (Pawn) 
	let _ = add_piece_to_board (g.board) ("7", "c") ("P1") (Black) ("Pawn") (Pawn) 
	(* white *)
	TEST = ((valid_move (pawn_white_1,("2","a"),("3","a")) g)=Valid(Basic))
	TEST = ((valid_move (pawn_white_1,("2","a"),("4","a")) g)=Valid(Basic))
	TEST = ((valid_move (pawn_white_1,("2","a"),("3","b")) g)=Invalid(MovementImpossible))
	TEST = ((valid_move (pawn_white_1,("3","b"),("7","b")) g)=Invalid(MovementImpossible))
	TEST = ((valid_move (pawn_white_1,("2","a"),("6","b")) g)=Invalid(MovementImpossible))
	(* black *)
	TEST = ((valid_move (pawn_black_3,("7","c"),("6","c")) g)=Valid(Basic))
	TEST = ((valid_move (pawn_black_3,("7","c"),("5","c")) g)=Valid(Basic))
	TEST = ((valid_move (pawn_black_3,("7","c"),("8","c")) g)=Invalid(MovementImpossible))
	TEST = ((valid_move (pawn_black_3,("7","c"),("9","c")) g)=Invalid(MovementImpossible))
	TEST = ((valid_move (pawn_black_3,("7","c"),("3","c")) g)=Invalid(MovementImpossible))

	(* Basic Knight Moves *)
	let g = make_empty_game()
	let _ = add_piece_to_board (g.board) ("5", "a") ("K1") (White) ("Knight") (Knight) 
	
	TEST = ((valid_move (knight_white_1,("5","a"),("6","c")) g)=Valid(Basic))
	TEST = ((valid_move (knight_white_1,("5","a"),("7","b")) g)=Valid(Basic))

	(* Basic Rook Moves *)
	let g = make_empty_game()
	let rook_white = { id="R1"; team=White; name="Rook"; piecetype=Rook; }
	let _ = add_piece_to_board (g.board) ("5", "d") ("R1") (White) ("Rook") (Rook) 

	TEST = ((valid_move (rook_white_1,("5","d"),("8","d")) g)=Valid(Basic))
	TEST = ((valid_move (rook_white_1,("5","d"),("5","g")) g)=Valid(Basic))
	TEST = ((valid_move (rook_white_1,("5","d"),("5","h")) g)=Valid(Basic))
	TEST = ((valid_move (rook_white_1,("5","d"),("6","h")) g)=Invalid(MovementImpossible))
	TEST = ((valid_move (rook_white_1,("5","d"),("7","h")) g)=Invalid(MovementImpossible))

	(* Basic Bishop Moves *)
	let g = make_empty_game()
	let bishop_white = { id="B1"; team=White; name="Bishop"; piecetype=Bishop; }
	let _ = add_piece_to_board (g.board) ("3", "d") ("B1") (White) ("Bishop") (Bishop)

	TEST = ((valid_move (bishop_white_1,("3","d"),("5","f")) g)=Valid(Basic))
	TEST = ((valid_move (bishop_white_1,("3","d"),("1","b")) g)=Valid(Basic))
	TEST = ((valid_move (bishop_white_1,("3","d"),("6","a")) g)=Valid(Basic))

	(* Basic Queen Moves *)
	let g = make_empty_game()
	let queen_white = { id="Q"; team=White; name="Queen"; piecetype=Queen; }
	let _ = add_piece_to_board (g.board) ("3", "d") ("Q") (White) ("Queen") (Queen)

	TEST = ((valid_move (queen_white_1,("3","d"),("8","d")) g)=Valid(Basic))
	TEST = ((valid_move (queen_white_1,("3","d"),("6","g")) g)=Valid(Basic))
	TEST = ((valid_move (queen_white_1,("3","d"),("1","b")) g)=Valid(Basic))

	(* Basic King Moves *)
	let g = make_empty_game()
	let _ = add_piece_to_board (g.board) ("3", "d") ("K") (White) ("King") (King)

	TEST = ((valid_move (king_white_1,("3","d"),("4","d")) g)=Valid(Basic))
	TEST = ((valid_move (king_white_1,("3","d"),("4","e")) g)=Valid(Basic))
	TEST = ((valid_move (king_white_1,("3","d"),("8","d")) g)=Invalid(MovementImpossible))

end


TEST_MODULE "valid_move with collisions" = struct
	
	let board_1 = demo_board_simple_1()
	let g = {make_empty_game() with board=board_1}

	(* queen should be blocked by the pawn *)
	TEST = ((valid_move (queen_white_1,("5","a"),("5","f")) g)=Invalid(MovementImpossible))
	
	(* queen should be unblocked *)
	TEST = ((valid_move (queen_white_1,("5","a"),("5","b")) g)=Valid(Basic))

	(* queen should be blocked by the bishop *)
	TEST = ((valid_move (queen_white_1,("5","a"),("7","d")) g)=Invalid(MovementImpossible))

	(* rook should be blocked by the pawn, and/or bishop *)
	TEST = ((valid_move (rook_white_1,("1","c"),("6","c")) g)=Invalid(MovementImpossible))
	TEST = ((valid_move (rook_white_1,("1","c"),("8","c")) g)=Invalid(MovementImpossible))

 	TEST = ((valid_move (pawn_black_1,("5","c"),("3","c")) g)=Valid(Basic))
	TEST = ((valid_move (pawn_black_1,("5","c"),("2","c")) g)=Invalid(MovementImpossible))
	TEST = ((valid_move (pawn_black_1,("5","c"),("1","c")) g)=Invalid(MovementImpossible))

	let _ = add_piece_to_board (g.board) ("1", "f") ("P1") (White) ("Pawn") (Pawn) 

	TEST = ((valid_move (pawn_white_1,("1","f"),("2","f")) g)=Valid(Basic))
	TEST = ((valid_move (pawn_white_1,("1","f"),("3","f")) g)=Valid(Basic))
	TEST = ((valid_move (pawn_white_1,("1","f"),("4","f")) g)=Invalid(MovementImpossible))
	TEST = ((valid_move (pawn_white_1,("1","f"),("5","f")) g)=Invalid(MovementImpossible))
	TEST = ((valid_move (pawn_white_1,("1","f"),("6","f")) g)=Invalid(MovementImpossible))
	TEST = ((valid_move (pawn_white_1,("1","f"),("7","f")) g)=Invalid(MovementImpossible)) 

	TEST = ((valid_move (pawn_white_1,("1","f"),("8","f")) g)=Invalid(MovementImpossible))
	TEST = ((valid_move (pawn_white_1,("1","f"),("9","f")) g)=Invalid(MovementImpossible))
 
end

(* let _ = print_endline "\nQueen White 1 Mvmts:" *)
(* 	let _ = print_mvmts mvmts
let _ = print_int (List.length(mvmts)) *)

TEST_MODULE "possible_movements" = struct

	(* --- demo_board_simple_1 --- *)
	let board_1 = demo_board_simple_1()
	let a_game = {make_empty_game() with board=board_1}

	(* Pawn Black 1 *)
	let mvmts = (possible_movements pawn_black_1 a_game)
	TEST = (List.length mvmts)=2
	TEST = (mvmts=[
			(pawn_black_1,("5","c"),("3","c"));
			(pawn_black_1,("5","c"),("4","c"))
		]) 
	(* Queen White 1 *)
	let mvmts = (possible_movements queen_white_1 a_game)
	TEST = (List.length mvmts)=14
	(* Bishop White 1 *)
	let mvmts = (possible_movements bishop_white_1 a_game)
	TEST = (List.length mvmts)=8

	(* --- full_board --- *)
	let board_full = make_init_board()
	let a_game = {make_empty_game() with board=board_full}
		
	(* Pawns - 2 initial moves *)
	let mvmts = (possible_movements pawn_white_1 a_game)
	TEST = (List.length mvmts)=2
	let mvmts = (possible_movements pawn_black_1 a_game)
	TEST = (List.length mvmts)=2
	let mvmts = (possible_movements pawn_white_3 a_game)
	TEST = (List.length mvmts)=2

	(* Queens - 0 initial moves *)
	let mvmts = (possible_movements queen_white_1 a_game)
	TEST = (List.length mvmts)=0
	let mvmts = (possible_movements queen_black_1 a_game)
	TEST = (List.length mvmts)=0

	(* Knights - 0 initial moves *)
	let mvmts = (possible_movements knight_white_1 a_game)
	TEST = (List.length mvmts)=2 
	let mvmts = (possible_movements knight_white_2 a_game)
	TEST = (List.length mvmts)=2

	(* Bishops - 0 initial moves *)
	let mvmts = (possible_movements bishop_white_1 a_game)
	TEST = (List.length mvmts)=0

	(* --- demo_board_dense_1 --- *)
	let board_dense_1 = demo_board_dense_1()
	let a_game = {make_empty_game() with board=board_dense_1}

	let mvmts = (possible_movements pawn_white_1 a_game)
	TEST = (List.length mvmts)=2

	let mvmts = (possible_movements queen_white_1 a_game)
	TEST = (List.length mvmts)=16

	let mvmts = (possible_movements rook_white_1 a_game)
	TEST = (List.length mvmts)=14

	let mvmts = (possible_movements rook_white_2 a_game)
	TEST = (List.length mvmts)=10
end

TEST_MODULE "pieces_capturable_by_moves" = struct

	let board_1 = demo_board_simple_1()
	let g = {make_empty_game() with board=board_1}

	let mvmts = possible_movements queen_white_1 g
	let vulnerable_pcs = pieces_capturable_by_moves mvmts board_1
	TEST = (List.length vulnerable_pcs)=1

	let mvmts = possible_movements rook_white_1 g
	let vulnerable_pcs = pieces_capturable_by_moves mvmts board_1
	TEST = (List.length vulnerable_pcs)=1

end

Pa_ounit_lib.Runtime.summarize ()








