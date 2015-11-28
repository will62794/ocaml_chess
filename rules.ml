open Chessmodel 

let valid_move (m:move) (brd:board) : bool = 
	failwith "valid_move unimplemented"

let possible_movements (p:piece) (brd:board) : move list = 
	failwith "possible_movements unimplemented"


type failtype = MovementImpossible | Disallowed
type validation = Valid | Invalid of failtype

let board_size = 8

(*    Movement Rules    *)

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

(* pawn movemnent rule parameterized on direction. move_dir = {1,-1}. 1 for White, -1 for Black move *)
let pawn_mvmt (x,y) (x',y') (move_dir:int) = 
	(x'-x,y'-y) = (0,move_dir)

let knight_mvmt (x,y) (x',y') = 
	(abs(x-x'),abs(y'-y))=(2,1) ||
	(abs(x-x'),abs(y'-y))=(1,2)

let bishop_mvmt (x,y) (x',y') = 
	diagonal_mvmt (x,y) (x',y') 1 board_size

let rook_mvmt (x,y) (x',y') = 
 	orthogonal_mvmt (x,y) (x',y') 1 board_size

let queen_mvmt (x,y) (x',y') = 
	diagonal_mvmt (x,y) (x',y') 1 board_size ||
	orthogonal_mvmt (x,y) (x',y') 1 board_size 

let king_mvmt (x,y) (x',y') = 
	orthogonal_mvmt (x,y) (x',y') 1 1 || 
	diagonal_mvmt (x,y) (x',y') 1 1 

(* let movement_valid (m:move) (b:board)= 
	failwith "none" *)
	(* ((src_x,src_y),(dest_x,dest_y)) = get_move_positions m *)


(* ------------------------------------------------------------ *)
(* ------ TESTS ----------------------------------------------- *)
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


end



