(*
 * The value 'eval board move' returns an integer representing the strength
 * of the inputted move.
 *
 * MOVES are evaluated rather than BOARD STATE for the sake of efficiency
 *
 * Move evaluation is based off of the following:
 * I. Captured pieces - point value of captured piece
 * II. Piece positioning - net gain in positional advantage
 * III. Piece vulnerabilities - number of and point values of vulnerable pieces
 * IV. Misc - misc, such as castled formation, piece is being moved, check,
 *        pawn moved to end, avoidance of check mate
 *
 * INVARIANT: THE INPUTTED MOVE --MUST-- BE A VALID MOVE
 *)

(* Notes to self: ask nate for pieces points
   remove positional points if piece is vulnerable?
   DETERMINE EARLY OR LATE GAME STATUS FOR KING'S POS POINTS
 *)

open Chessmodel
open Position_score

(* multipliers representing the weighting of the 3 board eval factors;
 * to be adjusted in testing *)
let capture_mult = 1.0
let position_mult = 1.0
let vulnerable_mult = 1.0
let misc_mult = 1.0

(* relative value of each piece- values derived from wikipedia *)
let get_piece_val (piece: piece): float =
  match piece.piecetype with
  | Pawn -> 100.
  | Knight -> 300.
  | Bishop -> 300.
  | Rook -> 500.
  | Queen -> 900.
  | King -> failwith ""

let get_piece (board: board) (loc: boardpos): piece option =
  snd (!get_square_on_board loc board)

(* points gathered from a piece capture *) (* change to use get piece *)
let capture_points (board: board) (move: move) : float =
  match move with
  | (_, _, dest) ->
   match !(get_square_on_board dest board) with
   | (_, pieceOp) ->
    match pieceOp with
    | None -> 0
    | Some piece -> get_piece_val piece

let position_points (board: board) (move: move) : float =
  match move with
  | (piece, src, dest) -> (get_pos_points piece.piecetype dest true) +
    (match get_piece board dest with
     | None -> 0
     | Some cap_piece -> get_pos_points cap_piece.piecetype dest true)

let vulnerable_points (board: board) (move: move) : float =
  match move with
  | (piece, _, dest) ->
    if (is_vulnerable dest board)
    then get_piece_val piece
    else 0.

let misc_points (board: board) (move: move) : float = 0.

let eval (board: board) (move: move) : float =
  (capture_mult * (capture_points board move)) +
  (position_mult * (position_points board move)) -
  (vulnerable_mult * (vulnerable_points board move)) +
  (misc_mult * (misc_points board move))