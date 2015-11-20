open Chessmodel 

let valid_move (m:move) (brd:board) : bool = 
	failwith "valid_move unimplemented"

let possible_movements (p:piece) (brd:board) : move list = 
	failwith "possible_movements unimplemented"


(* rule composition operator *)
let (>>>) = failwith "no compose operator yet"

type move_fxn = fun (int*int)

type rule_phys = move_fxn * string

(* 

TODO: IMPLEMENT RULES ENGINE 

Deadline:

Finish rules engine by November 30


Types of Rules:

1. Physical Piece Movements
	- Depends on:
		-> Current Position (X,Y)
		-> Destination (X',Y')
	- Will not factor in other board pieces

2. Game State Specific
	a. En Passant
	b. Castling
	c. Pawn Promotion
	d. Check, Checkmate

3. Capture Rules
	- Barring special rules, a piece can "capture" another piece if it can 
	validly move to that space as per positioning rules, and the piece is of opposing
	team



Rule Checking:

	physical |> game_state_rules |> capture rules |> bool








*)