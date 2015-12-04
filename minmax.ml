open Move_tree
open Chesstypes

let max lst: (float * move option) =
  List.fold_left (fun acc hd -> if (fst acc > fst hd)
                  then acc else hd) (min_float, None) lst

let min lst: (float * move option) =
  List.fold_left (fun acc hd -> if (fst acc < fst hd)
                  then acc else hd) (max_float, None) lst

(* make this tail recursive *)
let rec get_list_of_children (tree_list: move_tree list)
                              (levels: int) (team: team): (float * move option) list =
  match tree_list with
  | [] -> []
  | h::t -> match h with
            | Leaf -> failwith ""
            | Node ((score, game, move), nodes_tree_list) ->
                (min_max h levels team)::(get_list_of_children t levels team)

and min_max (tree: move_tree) (levels: int) (team: team): (float * move option) =
  match tree with
  | Leaf -> failwith ""
  | Node ((score, game, move), tree_list) ->
    if (levels = 0) then (score, move)
    else match team with
         | White -> max (get_list_of_children (tree_list) (levels-1) Black)
         | Black -> min (get_list_of_children (tree_list) (levels-1) White)