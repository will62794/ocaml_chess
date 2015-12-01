open Chesstypes 
open Chessmodel


(*  ---   Movement Rules  ---  *)

(* A diagonal movement of a distance that falls in range [near,far] *)
let diagonal_mvmt (x,y) (x',y') (near) (far) =
	let (dx,dy) =  ((x'-x),(y'-y)) in
	abs(dx)=abs(dy) && 
	abs(dx)>=near && abs(dx)<=far

(* An orthogonal movement (up, left, right, or down) of a 
 * distance that falls in range [near,far] *)
let orthogonal_mvmt (x,y) (x',y') (near) (far)  =
 	let (dx,dy) =  ((x'-x),(y'-y)) in
	(dx = 0 && abs(dy)>=near && abs(dy)<=far) ||
	(dy = 0 && abs(dx)>=near && abs(dx)<=far)	

let pawn_mvmt (x,y) (x',y') = 
	(x'-x,y'-y) = (0,1)

let knight_mvmt (x,y) (x',y') = 
	(abs(x-x'),abs(y'-y))=(2,1) ||
	(abs(x-x'),abs(y'-y))=(1,2)

let bishop_mvmt (x,y) (x',y') = 
	diagonal_mvmt (x,y) (x',y') 1 Chesstypes.brd_size

let rook_mvmt (x,y) (x',y') = 
 	orthogonal_mvmt (x,y) (x',y') 1 Chesstypes.brd_size

let queen_mvmt (x,y) (x',y') = 
	(diagonal_mvmt (x,y) (x',y') 1 Chesstypes.brd_size) ||
	(orthogonal_mvmt (x,y) (x',y') 1 Chesstypes.brd_size) 

let king_mvmt (x,y) (x',y') = 
	orthogonal_mvmt (x,y) (x',y') 1 1 || 
	diagonal_mvmt (x,y) (x',y') 1 1 

let mvmt_rules = 
	[
		(Pawn,(pawn_mvmt));
		(Knight,(knight_mvmt));
		(Bishop,(bishop_mvmt));
		(Rook,(rook_mvmt));
		(Queen,(queen_mvmt));
		(King,(king_mvmt))
	]

(* is a move within the 8x8 board *)
let inbounds_rule (m:move) : bool= 
	let (_,_,dst) = m in
	let (dst_x,dst_y) = boardpos_to_coords dst in
	let lower,upper = (1,brd_size) in
	(dst_x>=lower && dst_x<=upper && dst_y>=lower && dst_y<=upper)

let movement_rule (m:move) (b:board) : bool = 
	let (piece,s,d) = m in
	let (persp_s,persp_d) = 
		if piece.team = White then (s,d)
		else ((invert_boardpos s),(invert_boardpos d))
	in
	let (src,dst) = (boardpos_to_coords persp_s,boardpos_to_coords persp_d) in
	let mvmt_rule = (List.assoc piece.piecetype mvmt_rules) in
	(mvmt_rule (src) (dst))

(* is the move a capture move *)
let is_capture_move (brd:board) (m:move) =
	let (src_piece,src,dst) = m in
	let _,dst_piece = !(get_square_on_board dst brd) in
	match dst_piece with
		| None -> false
		| Some p -> 
			if p.team<>src_piece.team then true (* can only capture opponent piece *)
			else false

(*  ---   Collision Detection  ---  *)
	
let piece_at_sq sq = 
	let (pos,p)=sq in
	match (pos,p) with
	 | _,Some pce -> pce
	 | _,None -> failwith "no piece"

(* returns a list of the (x,y) positions that make up that path between start and end 
 * precondition: path can be made only diagonally or orthogonally.
 * note: does not include src and dst in the path  *)
let rec path_between src dst =
	let (srcx,srcy),(dstx,dsty) = src,dst in
	let next_x,next_y = (min (srcx+1) dstx),(min (srcy+1) dsty) in
	if (next_x,next_y)=dst 
		then []
	else 
		(next_x,next_y)::(path_between (next_x,next_y) dst)

(* Returns a list of pieces that lie on path from src to dest on brd *)
let pieces_in_way (src:boardpos) (dest:boardpos) (brd:board) : piece list =
	let (src,dest) = (boardpos_to_coords src,boardpos_to_coords dest) in
	let path = List.map coords_to_boardpos (path_between src dest) in
	(* let () = List.iter (fun (x,y) -> (Printf.printf "%s,%s\n" x y)) path in *)
	let sqs = List.map (fun pos -> !(get_square_on_board pos brd)) path in
	let occupied_sqs = List.filter (fun (pos,p) -> p<>None) sqs in
	(* let () = print_endline "" in *)
	(* let () = List.iter (fun p -> print_endline p.name) (List.map piece_at_sq occupied_sqs) in *)
	List.map piece_at_sq occupied_sqs

(* returns bool determining if executing the given move would collide with other pieces *)
let move_collisions (m:move) (brd:board) : bool = 
	let (p,src,dst) = m in
	let collisions = pieces_in_way src dst brd in
	(* let () = print_endline (string_of_bool ((List.length collisions)>0)) in *)
	(List.length collisions)>0

(* 
	Castling:
	a. 	King moves 2 squares towards a rook to its right, and the rook moves to the square the king crossed over
	b. 	May only be done if the king has never moved, the rook involved has never moved, the squares b/w
		the rook and king are not occupied
	-> treat castling as a king move
*)
let detect_castling (m:move) (g:game) : bool =
	let (p,src,dst) = m in
	let (x,y),(x',y') = (boardpos_to_coords src),(boardpos_to_coords dst) in 
	let king_dx = if (p.team = White) then 2 else -2 in
	let conditions = [
		p.piecetype = King;
		(y'=y);
		(x'-x)=king_dx
	] in
	List.fold_left (&&) true conditions


let detect_en_passant (m:move) (g:game) : bool = 
	false

let detect_pawn_promotion (m:move) (g:game) : bool = 
	false

let detect_special_move (m:move) (g:game) : bool = 
	let detectors = [
			(detect_castling m g); 
			(detect_en_passant m g); 
			(detect_pawn_promotion m g)
		] in
	List.fold_left (||) false detectors

(* assumes that the incoming move is a special move *)
let handle_special_move (m:move) (g:game) : move_validation = 
	if (detect_castling m g) then Valid(Castling)
	else if (detect_en_passant m g) then Valid(EnPassant)
	else if (detect_pawn_promotion m g) then Valid(PawnPromotion)
	else Invalid(MoveError)

let handle_normal_move (m:move) (g:game) : move_validation = 
	let brd = g.board in
	(* must pass all rules *)
	let mvmt_conds = [
		inbounds_rule m;
		movement_rule m brd;
		not (move_collisions m brd)
	] in
	let passed = List.fold_left (&&) true mvmt_conds in
	if passed then 
		if (is_capture_move brd m) then 
			Valid(Capture)
		else 
			Valid(Basic)
	else 
		Invalid(MovementImpossible)

let valid_move (m:move) (the_game:game) : move_validation =
	let special_move = detect_special_move m the_game in
	if special_move then 
		handle_special_move m the_game (* Special Move *)
	else
		handle_normal_move m the_game

		
(* --- Possible Moves for Piece (Inverse Rule Calculations) --- *)

let sq_to_coords (sq:square):int*int = 
	let pos,pce = sq in
	boardpos_to_coords pos

(* given a piece and its position, determine what squares in this row the piece could move to.
 * In other words, what positions in this row satisfy the pieces rule requirements  *)
let piece_moves_in_row (p:piece) (pos:boardpos) (g:game) (r:row): move list = 
	let sq_positions = List.map (fun (c,sq) -> fst(!sq)) r in
	let make_piece_move dst = (p,pos,dst) in
	let validate_pce_move pos = 
		match (valid_move (make_piece_move pos) g) with
		 | Valid t -> true
		 | Invalid t -> false 
	in
	let move_positions = List.filter (fun pos -> (validate_pce_move pos)) sq_positions in
	List.map (fun pos -> make_piece_move pos) move_positions


let possible_movements (p:piece) (g:game) : move list = 
	let brd = g.board in
	let piece_pos = 
		match find_piece_pos p brd with
		 | Some pos -> pos
		 | None -> failwith "PieceNotFound" 
	in
	let _,board_rows = List.split brd in
	let row_moves = List.map (piece_moves_in_row p piece_pos g) board_rows in
	List.flatten row_moves

let pieces_capturable_by_moves (moves:move list) (brd:board) : piece list = 
	let capture_moves = List.filter (is_capture_move brd) moves in
	let capturable_sqs = List.map (fun (p,s,d) -> !(get_square_on_board d brd)) capture_moves in
	List.map piece_at_sq capturable_sqs



(* ---------------------------------------------------------------------------- *)
(* ---------------------------------------------------------------------------- *)
(* ----------------------- TESTS ---------------------------------------------- *)
(* ---------------------------------------------------------------------------- *)
(* ---------------------------------------------------------------------------- *)

TEST_MODULE "piece_movement_rule_tests" = struct

	TEST = (orthogonal_mvmt (3,3) (3,4) 1 Chesstypes.brd_size)=true
	TEST = (orthogonal_mvmt (3,3) (3,2) 1 Chesstypes.brd_size)=true
	TEST = (orthogonal_mvmt (3,3) (3,1) 1 Chesstypes.brd_size)=true
	TEST = (orthogonal_mvmt (3,3) (4,3) 1 Chesstypes.brd_size)=true
	TEST = (orthogonal_mvmt (3,3) (2,3) 1 Chesstypes.brd_size)=true


	(* Pawn *)
	TEST = (pawn_mvmt (3,3) (3,4))=true
	TEST = (pawn_mvmt (5,2) (5,3))=true
	TEST = (pawn_mvmt (3,3) (3,2))=false
	TEST = (pawn_mvmt (3,3) (3,8))=false

	(* Knight *)
	TEST = (knight_mvmt (3,3) (5,4))=true
	TEST = (knight_mvmt (3,3) (5,2))=true
	TEST = (knight_mvmt (3,3) (4,5))=true
	TEST = (knight_mvmt (3,3) (4,1))=true
	TEST = (knight_mvmt (3,3) (1,4))=true
	TEST = (knight_mvmt (3,3) (1,2))=true
	TEST = (knight_mvmt (3,3) (2,5))=true
	TEST = (knight_mvmt (3,3) (2,1))=true
	TEST = (knight_mvmt (5,5) (8,8))=false
	TEST = (knight_mvmt (7,7) (1,1))=false
	TEST = (knight_mvmt (3,3) (6,6))=false

	(* Rook *)
	TEST = (rook_mvmt (3,3) (6,3))=true
	TEST = (rook_mvmt (0,0) (0,5))=true
	TEST = (rook_mvmt (1,5) (1,4))=true
	TEST = (rook_mvmt (1,5) (1,3))=true
	TEST = (rook_mvmt (1,5) (1,2))=true
	TEST = (rook_mvmt (1,5) (1,1))=true


	(* Bishop *)
	TEST = (bishop_mvmt (3,3) (6,6))=true
	TEST = (bishop_mvmt (2,2) (0,0))=true
	TEST = (bishop_mvmt (3,3) (1,5))=true
	TEST = (bishop_mvmt (3,3) (1,1))=true
	TEST = (bishop_mvmt (3,3) (5,5))=true
	TEST = (bishop_mvmt (3,3) (5,1))=true

	(* Queen *)
	TEST = (queen_mvmt (3,3) (7,3))=true
	TEST = (queen_mvmt (3,3) (1,1))=true
	TEST = (queen_mvmt (3,3) (6,3))=true
	TEST = (queen_mvmt (1,5) (1,4))=true
	TEST = (queen_mvmt (1,5) (1,3))=true
	TEST = (queen_mvmt (1,5) (1,2))=true
	TEST = (queen_mvmt (1,5) (1,1))=true
	TEST = (queen_mvmt (3,3) (5,4))=false

	(* King *)
	TEST = (king_mvmt (3,3) (4,3))=true
	TEST = (king_mvmt (3,3) (2,2))=true
	TEST = (king_mvmt (3,3) (6,6))=false



	(* General *)
	let piece = {
		id="P1";
		team=White;
		name="Pawn";
		piecetype=Pawn;
	}

	let m = (piece,("5","a"),("6","a"))
	let b = make_empty_board()
	TEST = (movement_rule m b)=true


end

TEST_MODULE "path_collisions" = struct
	
	(* path_between *)
	TEST = path_between (2,2) (4,4) = [(3,3)]
	TEST = path_between (2,2) (2,6) = [(2,3);(2,4);(2,5)]
	TEST = path_between (1,4) (6,4) = [(2,4);(3,4);(4,4);(5,4)]

	let b = make_empty_board()
	let src,dst = ("1","a"),("2","a")
	TEST = (pieces_in_way src dst b)=[]

	let b = make_init_board()
	let src,dst = ("1","a"),("8","a")
	let pieces = (pieces_in_way src dst b)


	(* Basic Queen Moves *)
	let brd = make_empty_board()
	let queen_white = { id="Q1"; team=White; name="Queen"; piecetype=Queen; }
	let pawn_white = { id="P3"; team=White; name="Pawn"; piecetype=Pawn; }
	let _ = add_piece_to_board (brd) ("3", "d") ("Q1") (White) ("Queen") (Queen)
	let _ = add_piece_to_board (brd) ("3", "e") ("P3") (White) ("Pawn") (Pawn) 

	let src,dst = ("3","d"),("3","h")
	let pieces = (pieces_in_way src dst brd)
	TEST = (pieces=[pawn_white])

end




