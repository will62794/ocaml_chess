(*

	Application wide chess specific type definitions

*)

(* standard chess board size *)
let brd_size = 8

type team = White | Black

type piecekind =
	| Pawn
	| Knight
	| Bishop
	| Rook
	| King
	| Queen

type piece = {
		id: string; (* each piece will be identified by a unique id e.g. P4 or B2 *)
		team: team; (* black or white *)
		name: string; (* piece type *)
		piecetype: piecekind;
	}

type boardpos = string * string (* (row,column) e.g. (5,a) *)
type square = boardpos * piece option (* (position,piece); piece=None if the suare is empty *)
type row = (string * square ref) list (* (row #, list of squares) *)
type board = (string * row) list

type move = piece * boardpos * boardpos  (* represents a physical move --> (piece, src, dest) *)
type failtype = MovementImpossible | MoveError | WouldBeCheck
type movetype = Basic | Capture | EnPassant | CastlingLeft | CastlingRight | PawnPromotion
type move_validation = Valid of movetype | Invalid of failtype

type checktype = Check | Checkmate

type player = string

type game = {
	board: board;
	players: player * player;
	total_moves: int;
	current_turn: team;
	in_enpassant: piece option;
	did_castle: bool * bool; (* did_white_castle_yet, did_black_castle_yet *)
	moved_pieces: piece list;
}

type difficulty_level = int