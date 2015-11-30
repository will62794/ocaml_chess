open Eval
open Chesstypes
open rules



type move_tree =
  | Leaf
  | Node of (float * board) * move_tree list


let rec generate_tree (board:board) (levels: int)=
  if levels = 0 then Leaf else
  let possible_moves = get_all_possible_moves board in 
  let  

