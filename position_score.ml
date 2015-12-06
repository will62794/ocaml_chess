(*
 * The position_score scores the positional advantages gained or lost
 * through the movement/capture of a piece. Pawns, knights, bishops,
 * and the king have tables representing the strength of the position
 * of the piece.
 *
 * While the idea to score positional advantages was our own idea, the
 * scoring of the positions of the pieces were taken from online (as
 * our group's knowledge of chess is limited). The table scoring was
 * taken from the following link:
 * http://www.chessbin.com/post/Piece-Square-Table
 *)

open Chessmodel
open Chesstypes

type pos_board = float array array

let rec create_board (board: pos_board) (vals: float list)
  (row: int) (col: int): pos_board =
  match vals with
  | [] -> board
  | h::t -> board.(col).(row) <- h;
        if (col = 7) then create_board board t (row-1) 0
        else create_board board t row (col+1)

let pawn_vals = [ 75.;  75.;  75.;  75.;  75.;  75.;  75.;  75.;
          50.; 50.; 50.; 50.; 50.; 50.; 50.; 50.;
          10.; 10.; 20.; 30.; 30.; 20.; 10.; 10.;
           5.;  5.; 10.; 27.; 27.; 10.;  5.;  5.;
           0.;  0.;  0.; 25.; 25.;  0.;  0.;  0.;
           5.; -5.;-10.;  0.;  0.;-10.; -5.;  5.;
           5.; 10.; 10.;-25.;-25.; 10.; 10.;  5.;
           0.;  0.;  0.;  0.;  0.;  0.;  0.;  0.]

let knight_vals = [-50.;-40.;-30.;-30.;-30.;-30.;-40.;-50.;
            -40.;-20.;  0.;  0.;  0.;  0.;-20.;-40.;
            -30.;  0.; 10.; 15.; 15.; 10.;  0.;-30.;
            -30.;  5.; 15.; 20.; 20.; 15.;  5.;-30.;
            -30.;  0.; 15.; 20.; 20.; 15.;  0.;-30.;
            -30.;  5.; 10.; 15.; 15.; 10.;  5.;-30.;
            -40.;-20.;  0.;  5.;  5.;  0.;-20.;-40.;
            -50.;-40.;-20.;-30.;-30.;-20.;-40.;-50.]

let bishop_vals = [-20.;-10.;-10.;-10.;-10.;-10.;-10.;-20.;
            -10.;  0.;  0.;  0.;  0.;  0.;  0.;-10.;
            -10.;  0.;  5.; 10.; 10.;  5.;  0.;-10.;
            -10.;  5.;  5.; 10.; 10.;  5.;  5.;-10.;
            -10.;  0.; 10.; 10.; 10.; 10.;  0.;-10.;
            -10.; 10.; 10.; 10.; 10.; 10.; 10.;-10.;
            -10.;  5.;  0.;  0.;  0.;  0.;  5.;-10.;
            -20.;-10.;-40.;-10.;-10.;-40.;-10.;-20.]

let king_vals_early = [-30.; -40.; -40.; -50.; -50.; -40.; -40.; -30.;
               -30.; -40.; -40.; -50.; -50.; -40.; -40.; -30.;
               -30.; -40.; -40.; -50.; -50.; -40.; -40.; -30.;
               -30.; -40.; -40.; -50.; -50.; -40.; -40.; -30.;
               -20.; -30.; -30.; -40.; -40.; -30.; -30.; -20.;
                 -10.; -20.; -20.; -20.; -20.; -20.; -20.; -10.;
                20.;  20.;   0.;   0.;   0.;   0.;  20.;  20.;
              20.;  30.;  10.;   0.;   0.;  10.;  30.;  20.]

let king_vals_late = [-50.;-40.;-30.;-20.;-20.;-30.;-40.;-50.;
              -30.;-20.;-10.;  0.;  0.;-10.;-20.;-30.;
              -30.;-10.; 20.; 30.; 30.; 20.;-10.;-30.;
              -30.;-10.; 30.; 40.; 40.; 30.;-10.;-30.;
              -30.;-10.; 30.; 40.; 40.; 30.;-10.;-30.;
              -30.;-10.; 20.; 30.; 30.; 20.;-10.;-30.;
              -30.;-30.;  0.;  0.;  0.;  0.;-30.;-30.;
              -50.;-30.;-30.;-30.;-30.;-30.;-30.;-50.]

let pawn_board = create_board (Array.make_matrix 8 8 0.) pawn_vals 7 0
let knight_board = create_board (Array.make_matrix 8 8 0.) knight_vals 7 0
let bishop_board = create_board (Array.make_matrix 8 8 0.) bishop_vals 7 0
let king_board_early = create_board (Array.make_matrix 8 8 0.) king_vals_early 7 0
let king_board_late = create_board (Array.make_matrix 8 8 0.) king_vals_late 7 0

let get_pos_points (piece: piece) (move: boardpos) (early: bool): float =
  let pos = (match piece.team with
        | White -> ((get_int_of_letter (snd move)) - 1, (int_of_string (fst move)) - 1)
        | Black -> ((get_int_of_letter (snd move)) - 1, 8 - (int_of_string (fst move)))) in
  match piece.piecetype with
  | Pawn -> pawn_board.(fst pos).(snd pos)
  | Knight -> knight_board.(fst pos).(snd pos)
  | Bishop -> bishop_board.(fst pos).(snd pos)
  | Rook -> 0.
  | King -> if early then king_board_early.(fst pos).(snd pos)
        else king_board_late.(fst pos).(snd pos)
  | Queen -> 0.