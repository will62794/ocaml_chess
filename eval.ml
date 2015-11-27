(* 
 * The value 'eval board move' returns an integer representing the strength 
 * of the inputted move. Positive values represent strong moves for
 * white, while negative values represent strong moves for black.
 *
 * MOVES are evaluated rather than BOARD STATE for the sake of efficiency
 *
 * Move evaluation is based off of the following:
 * I. Captured pieces - point value of captured piece
 * II. Piece positioning - net gain in positional advantage
 * III. Piece vulnerabilities - number of and point values of vulnerable pieces
 * IV. Misc - misc stuff, such as castled formation, which piece is being moved
 *
 * INVARIANT: THE INPUTTED MOVE --MUST-- BE A VALID MOVE
 *)

(* Notes to self: ask nate for captured pieces thing *)

open chessmodel

(* multipliers representing the weighting of the 3 board eval factors;
 * to be adjusted in testing *)
let capture_mult = 1.0
let position_mult = 1.0
let vulnerable_mult = 1.0
let misc_mult = 1.0

let capture_points (board: board) (move: move) : int =
	match move with
	| (_, _, dest) ->
	 match get_square_on_board dest board with
	 | (_, piece) ->


let position_points (board: board) (move: move) : int = failwith "TODO"

let vulnerable_points (board: board) (move: move) : int = failwith "TODO"

let misc_points (board: board) (move: move) : int = failwith "TODO"

let eval (board: board) (move: move) : int = 
	(capture_mult * (capture_points board move)) + 
	(position_mult * (position_points board move)) + 
	(vulnerable_mult * (vulnerable_points board move)) + 
	(misc_mult * (misc_points board move))