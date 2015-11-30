open Eval
open Chesstypes
open rules



type move_tree =
  | Leaf
  | Node of (float * board) * board list


let rec generate_tree board levels=
  if levels = 0

