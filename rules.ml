open Chesstypes 
open Chessmodel
open Game

type direction = Left | Right

(* -------------------------------- *)
(*  ---   Movement Rules  --------  *)
(* -------------------------------- *)

type mvmt_rule_type = Normal | Capturing | Initial

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

(* mvmt rule for pawn that has never moved *)
let pawn_initial_mvmt (x,y) (x',y') =
	(x'-x,y'-y) = (0,2) ||
	(x'-x,y'-y) = (0,1)

(* mvmt rule for pawn that is taking another piece *)
let pawn_capture_mvmt (x,y) (x',y') = 
	(x'-x,y'-y) = (1,1) ||
	(x'-x,y'-y) = (-1,1)

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

(*  There is a lot of redundancy here, but it is simpler to make each piece's movement rules
 *	explicity for each game situation. Pawns are really the only pieces that utilize this functionality
 *	but it allows for a nice, general description of each piece's move rules.
 *
 *  Normal -> normal, non special move to an empty space
 * 	Capturing -> piece is moving to a space occupied by an opponent's piece
 *	Initial -> same as Normal, except that the move is the first one taken by that piece in a game
*)

let pawn_mvmt_rules = [
		(Normal,pawn_mvmt);
		(Capturing,pawn_capture_mvmt);
		(Initial,pawn_initial_mvmt)
	]

let knight_mvmt_rules = [ 
		(Normal,knight_mvmt);
		(Capturing,knight_mvmt);
		(Initial,knight_mvmt)
	]

let bishop_mvmt_rules = [ 
		(Normal,bishop_mvmt);
		(Capturing,bishop_mvmt);
		(Initial,bishop_mvmt)
	]

let rook_mvmt_rules = [ 
		(Normal,rook_mvmt);
		(Capturing,rook_mvmt);
		(Initial,rook_mvmt)
	]
let queen_mvmt_rules = [ 
		(Normal,queen_mvmt);
		(Capturing,queen_mvmt);
		(Initial,queen_mvmt)
	]

let king_mvmt_rules = [ 
		(Normal,king_mvmt);
		(Capturing,king_mvmt);
		(Initial,king_mvmt)
	]

let mvmt_rules_table = [
		(Pawn,pawn_mvmt_rules);
		(Knight,knight_mvmt_rules);
		(Bishop,bishop_mvmt_rules);
		(Rook,rook_mvmt_rules);
		(Queen,queen_mvmt_rules);
		(King,king_mvmt_rules)
	]

(* a utility function that takes the (src,dest) positions of 
 * a move and turns it around so that the coordinates read as if from the 
 * player/team's persepctive. that is, the player(team) who is making the move.
 * for example, a move from (a5 -> c7) by a Bishop, would be viewed as 
 *		(a5 -> c7) by a White Team
 *		(h3 -> f1) by a Black Team
 *	this is mainly helpful in simplifying Pawn movement calculations, 
 *	since they are pieces that can only ever move forward, but "forward" is
 *	different from each team's point of view
*)
let perspectify_move (m:move) = 
	let (piece,s,d) = m in
	if piece.team = White 
		then (s,d)
	else 
		((invert_boardpos s),(invert_boardpos d))

(* given an association list that maps piecekinds to 
 * movement rules, checks if a move passes the rule for the piece
 * involved in that move *)
let movement_rule (m:move) (g:game) (mtype:mvmt_rule_type) : bool = 
	let (piece,s,d) = m in
	let (persp_s,persp_d) = perspectify_move m in
	let (src,dst) = (boardpos_to_coords persp_s,boardpos_to_coords persp_d) in
	let piece_mvmt_rules = (List.assoc piece.piecetype mvmt_rules_table) in
	let mvmt_rule = (List.assoc mtype piece_mvmt_rules) in
	(mvmt_rule (src) (dst))


(* is a move within the 8x8 board *)
let inbounds_rule (m:move) : bool= 
	let (_,_,dst) = m in
	let (dst_x,dst_y) = boardpos_to_coords dst in
	let lower,upper = (1,brd_size) in
	( (dst_x>=lower) && (dst_x<=upper) && 
	  (dst_y>=lower) && (dst_y<=upper) )

(* is the move a capture move *)
let is_capture_move (brd:board) (m:move) =
	let (src_piece,src,dst) = m in
	let _,dst_piece = !(get_square_on_board dst brd) in
	match dst_piece with
		| None -> false
		| Some p -> 
			if p.team<>src_piece.team then true (* can only capture opponent piece *)
			else false

(* ------------------------------------- *)
(*  ---   Collision Detection  --------- *)
(* ------------------------------------- *)

(* is a board square currently occupied by a certain team *)
let space_occupied (p:boardpos) (g:game) (t:team) =
	match !(get_square_on_board p g.board) with
		| (pos,Some p) -> 
			if p.team = t 
				then true
				else false
		| (pos,None) -> false

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
	-> dir tells whether to check castling to Left or Right
*)
let detect_castling (m:move) (g:game) (dir:direction) : bool =
	let (p,src,dst) = m in
	let white_castled,black_castled = g.did_castle in
	let can_castle = not (if (p.team = White) then white_castled else black_castled) in
	let (x,y),(x',y') = (boardpos_to_coords src),(boardpos_to_coords dst) in 
	let side_swap = (if dir=Left then 1 else -1) in (* castle left or right *)
	let king_dx = side_swap * (if (p.team = White) then 2 else -2) in
	let conditions = [
		p.piecetype = King;
		can_castle;
		(y'=y);
		(x'-x)=king_dx
	] in
	List.fold_left (&&) true conditions

let detect_en_passant (m:move) (g:game) : bool = 
	false

let detect_pawn_promotion (m:move) (g:game) : bool = 
	let (p,src,dst) = m in
	let (x,y),(x',y') = (boardpos_to_coords src),(boardpos_to_coords dst) in 
	let goal_y = if p.team=White then brd_size else 1 in
	let conditions = [
			p.piecetype = Pawn;
			x'=x;
			y'=goal_y;
		]
	in
	List.fold_left (&&) true conditions

let detect_special_move (m:move) (g:game) : bool = 
	let detectors = [
			(detect_castling m g Left); 
			(detect_castling m g Right); 
			(detect_en_passant m g); 
			(detect_pawn_promotion m g)
		] in
	List.fold_left (||) false detectors

(* Normal, Initial, or Capturing *)
let get_mvmt_type (m:move) (g:game) =  
	let (pce,src,dst) = m in
	if is_capture_move g.board m then Capturing
	else if not (piece_moved_yet pce g) then Initial
	else Normal 

let check_mvmt_rules (m:move) (g:game) : bool = 
	let (pce,src,dst) = m in
	let mvmt_type = (get_mvmt_type m g) in
	(* check rules *)
	let mvmt_conds = [
		not (space_occupied dst g pce.team); (* [RULE] dst square must not be occupied by pce's team *)
		movement_rule m g mvmt_type; 		 (* [RULE] piece obeys its movement constraints  *)
		not (move_collisions m g.board) 	 (* [RULE] the move does not collide w/ other pieces  *)
	] 
	in
	List.fold_left (&&) true mvmt_conds

(* assumes that the incoming move is a special move *)
let handle_special_move (m:move) (g:game) : move_validation = 
	if (detect_castling m g Left) then Valid(CastlingLeft)
	else if (detect_castling m g Right) then Valid(CastlingRight)
	else if (detect_en_passant m g) then Valid(EnPassant)
	else if (detect_pawn_promotion m g) then Valid(PawnPromotion)
	else Invalid(MoveError)

let handle_normal_move (m:move) (g:game) : move_validation = 
	(* detect capture *)
	let mvmt_type = (get_mvmt_type m g) in
	if (check_mvmt_rules m g) then 
		if (mvmt_type = Capturing)
			then
				let _ = print_endline "capture move" in
				Valid(Capture)
			else 
				Valid(Basic)
	else 
		Invalid(MovementImpossible)

let valid_move (m:move) (the_game:game) : move_validation =
	(* universal rule - piece moves to valid board space  *)
	if not (inbounds_rule m) then 
		Invalid(MovementImpossible) 
	else
		if (detect_special_move m the_game) then 
			handle_special_move m the_game (* Special Move *)
		else
			handle_normal_move m the_game (* Normal Move *)

		
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
		let (dst_x,dst_y) = pos in
		match (valid_move (make_piece_move pos) g) with
		 | Valid t -> 
		 	let _ = (Printf.printf "[%s,%s] " dst_x dst_y) in true
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
	(* orthogonal *)
	TEST = (orthogonal_mvmt (3,3) (3,4) 1 Chesstypes.brd_size)=true
	TEST = (orthogonal_mvmt (3,3) (3,2) 1 Chesstypes.brd_size)=true
	TEST = (orthogonal_mvmt (3,3) (3,1) 1 Chesstypes.brd_size)=true
	TEST = (orthogonal_mvmt (3,3) (4,3) 1 Chesstypes.brd_size)=true
	TEST = (orthogonal_mvmt (3,3) (2,3) 1 Chesstypes.brd_size)=true
	
	(* diagonal *)
	TEST = (diagonal_mvmt (1,5) (2,6) 1 Chesstypes.brd_size)=true
	TEST = (diagonal_mvmt (3,4) (6,7) 1 Chesstypes.brd_size)=true
	TEST = (diagonal_mvmt (1,1) (8,8) 1 Chesstypes.brd_size)=true
	TEST = (diagonal_mvmt (2,2) (1,1) 1 Chesstypes.brd_size)=true
	TEST = (diagonal_mvmt (2,2) (3,3) 1 Chesstypes.brd_size)=true
	TEST = (diagonal_mvmt (2,2) (3,1) 1 Chesstypes.brd_size)=true
	TEST = (diagonal_mvmt (2,2) (1,3) 1 Chesstypes.brd_size)=true
	TEST = (diagonal_mvmt (1,5) (1,8) 1 Chesstypes.brd_size)=false

	(* Pawn *)
	TEST = (pawn_mvmt (3,3) (3,4))=true
	TEST = (pawn_mvmt (5,2) (5,3))=true
	TEST = (pawn_mvmt (3,3) (3,2))=false
	TEST = (pawn_mvmt (3,3) (3,8))=false
	TEST = (pawn_mvmt (3,3) (3,5))=false
	TEST = (pawn_mvmt (3,3) (3,6))=false

	TEST = (pawn_capture_mvmt (3,3) (4,4))=true
	TEST = (pawn_capture_mvmt (3,3) (2,4))=true
	TEST = (pawn_capture_mvmt (3,3) (3,4))=false
	TEST = (pawn_capture_mvmt (3,3) (3,5))=false
	TEST = (pawn_capture_mvmt (3,3) (3,6))=false
	TEST = (pawn_capture_mvmt (3,3) (3,7))=false

	TEST = (pawn_initial_mvmt (2,2) (2,4))=true
	TEST = (pawn_initial_mvmt (4,2) (4,4))=true
	TEST = (pawn_initial_mvmt (4,2) (4,3))=true
	TEST = (pawn_initial_mvmt (4,2) (4,5))=false
	TEST = (pawn_initial_mvmt (3,3) (2,4))=false
	TEST = (pawn_initial_mvmt (3,3) (3,6))=false
	TEST = (pawn_initial_mvmt (3,3) (3,7))=false
	TEST = (pawn_initial_mvmt (3,3) (3,8))=false


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


end

TEST_MODULE "perspectify move" = struct

	let pawn = { id="P3"; team=Black; name="Pawn"; piecetype=Pawn; }
	TEST = (perspectify_move (pawn,("3","d"),("3","h"))) = (("6","e"),("6","a"))
	TEST = (perspectify_move (pawn,("5","b"),("7","e"))) = (("4","g"),("2","d"))

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




