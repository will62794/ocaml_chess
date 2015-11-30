open Chesstypes 
open Chessmodel

type failtype = MovementImpossible | Disallowed
type validation = Valid | Invalid of failtype

(*  ---   Movement Rules  ---  *)

(* A diagonal movement of a distance that falls in range [near,far] *)
let diagonal_mvmt (x,y) (x',y') (near) (far) =
	let (dx,dy) =  ((x'-x),(y'-y)) in
	abs(dx)=abs(dy) && 
	abs(dx)>=near && abs(dx)<=far

(*  An orthogonal movement (up, left, right, or down) of a 
	distance that falls in range [near,far] *)
let orthogonal_mvmt (x,y) (x',y') (near) (far)  =
 	let (dx,dy) =  ((x'-x),(y'-y)) in
	(dx = 0 && dy>=near && dy<=far) ||
	(dy = 0 && dx>=near && dx<=far)	

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
	diagonal_mvmt (x,y) (x',y') 1 Chesstypes.brd_size ||
	orthogonal_mvmt (x,y) (x',y') 1 Chesstypes.brd_size 

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

let movement_rule (m:move) (b:board) = 
	let (piece,s,d) = m in
	let (src,dst) = (boardpos_to_coords s,boardpos_to_coords d) in
	let mvmt_rule = (List.assoc piece.piecetype mvmt_rules) in
	mvmt_rule (src) (dst)


(*  ---   Collision Detection  ---  *)
		
(* 
	returns a list of the (x,y) positions that make up that path between start and end 
	precondition: path can be made only diagonally or orthogonally  
*)
let rec path_between src dst =
	if src=dst then [] 
	else
		let (srcx,srcy),(dstx,dsty) = src,dst in
		let next_x,next_y = (min (srcx+1) dstx),(min (srcy+1) dsty) in
		(next_x,next_y)::(path_between (next_x,next_y) dst)

(* Returns a list of pieces that lie on path from src to dest on brd*)
let pieces_in_way (src:boardpos) (dest:boardpos) (brd:board) : piece list =
	let (src,dest) = (boardpos_to_coords src,boardpos_to_coords dest) in
	let path = List.map coords_to_boardpos (path_between src dest) in
	let sqs = List.map (fun pos -> !(get_square_on_board pos brd)) path in
	let occupied_sqs = List.filter (fun (pos,p) -> p<>None) sqs in
	let piece_at_sq sq = 
		let (pos,p)=sq in
		match pos,p with
		| _,Some pce -> pce
		| _,None -> failwith "no piece" 
	in
	List.map piece_at_sq occupied_sqs


let is_vulnerable_move m brd = 
	false

let is_vulnerable_pos pos brd = 
	false

let is_vulnerable p brd = 
	false

let valid_move (m:move) (brd:board) : bool = 
	failwith "valid_move unimplemented"

let possible_movements (p:piece) (brd:board) : move list = 
	failwith "possible_movements unimplemented"




(* ------------------------------------------------------------ *)
(* ------------------------------------------------------------ *)
(* ------ TESTS ----------------------------------------------- *)
(* ------------------------------------------------------------ *)
(* ------------------------------------------------------------ *)

TEST_MODULE "piece_movement_rule_tests" = struct

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
	TEST = path_between (2,2) (4,4) = [(3,3);(4,4)]
	TEST = path_between (2,2) (2,6) = [(2,3);(2,4);(2,5);(2,6)]
	TEST = path_between (1,4) (6,4) = [(2,4);(3,4);(4,4);(5,4);(6,4)]

	let b = make_empty_board()
	let src,dst = ("1","a"),("2","a")
	TEST = (pieces_in_way src dst b)=[]

	let b = make_init_board()
	let src,dst = ("1","a"),("8","a")
	let pieces = (pieces_in_way src dst b)


(* 	let x,y = boardpos_to_coords ("1","c")

	let _ = print_int x
	let _ = print_int y
	let k = coords_to_boardpos (1,1)
	let x,y = k *)

(* 	let _ = print_endline (fst k)
	let _ = print_endline (snd k) *)


end




