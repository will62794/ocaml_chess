
(* Application-wide type defintions for CamlChess *)

type game = {
	board: int list list;
	score: int;
	players: string list;
}

(* "move" type (?) *)


(* "boardsquare" type (?) *)


(* "board" type (?) *)


(* "player" type (?) *)


(* "piece" types (variant?) *)


type chesspiece = 
	|Pawn
	|Rook
	|Castle
	|Bishop
	|Knight
	|Queen
	|King
			