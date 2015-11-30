open Eval
open Chesstypes
open rules



type move_tree =
  | Leaf
<<<<<<< HEAD
  | Node of (float * board) * board list


let rec generate_tree board levels=
  if levels = 0
=======
  | Node of (float * board) * move_tree list


let rec generate_tree (board:board) (levels: int)=
  if levels = 0 then Leaf else
  let possible_moves = get_all_possible_moves board in 
  let  
>>>>>>> e3e5033d7e52036556e0c78a75c94fc8b4ba1122

