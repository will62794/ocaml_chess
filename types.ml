
(* Application-wide type defintions for CamlChess *)

(* "piece" types (variant?) *)

type position = int * int
type piece = string * position

type chesspiece = 
	|Pawn of piece
	|Rook of piece
	|Bishop of piece
	|Knight of piece
	|Queen of piece
	|King of piece

(* "move" type (?) *)
type movement = int*int
type move = chesspiece * movement

(* "boardsquare" type (?) *)
type boardsquare = chesspiece option

(* "board" type (?) *)
type board = boardsquare list list

(* "player" type (?) *)
type player = string

type game = {
	board: board;
	players: player list;
}
(*Represents the difficulty level invariant of 1 through 10. 1 being lowest ai difficulty. 10 being highest ai difficulty*)
type difficulty_level = int
			