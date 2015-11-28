(* 

	Application wide chess specific type definitions

*)

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
type move = piece * boardpos * boardpos  (* represents a PHYSICAL move --> (piece, src, dest) *)
type square = boardpos * piece option (* (position,piece); piece=None if the suare is empty *)
type row = (string * square ref) list (* (row #, list of squares) *)
type board = (string * row) list

(* type player = string

type game = {
	board: board;
	players: player list;
}
Represents the difficulty level invariant of 1 through 10. 1 being lowest ai difficulty. 10 being highest ai difficulty
type difficulty_level = int

 *)