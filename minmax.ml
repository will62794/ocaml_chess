open Move_tree
open Chesstypes

let max lst: (float * move option) =
  List.fold_left (fun acc hd -> if (fst acc > fst hd)
                  then acc else hd) (min_float, None) lst

let min lst: (float * move option) =
  List.fold_left (fun acc hd -> if (fst acc < fst hd)
                  then acc else hd) (max_float, None) lst


(*

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
    if (levels = 1) then (score, move)
    else match team with
         | White -> max (get_list_of_children (tree_list) (levels-1) Black)
         | Black -> min (get_list_of_children (tree_list) (levels-1) White)

*)



(*type move_tree =
  | Leaf
  | Node of (float * game * move option) * move_tree list*)



let min_1 (a_list: (float*move) list) : (float*move) =
  let x = List.sort (fun a b -> if (fst a = fst b) then 0 else if (fst a > fst b) then 1 else -1 ) a_list in
  List.hd x


let max_1 (a_list: (float*move) list) : (float*move) =
  let x = List.sort (fun a b -> if (fst a = fst b) then 0 else if (fst a < fst b) then 1 else -1 ) a_list in
  List.hd x


let min_2 (a_list: float list) : float =
  let x = List.sort (fun a b -> if (a = b) then 0 else if (a > b) then 1 else -1 ) a_list in
  List.hd x

let max_2 (a_list: float list) : float =
  let x = List.sort (fun a b -> if (a = b) then 0 else if (a < b) then 1 else -1) a_list in
  List.hd x


let get_other_team (team:team) =
  match team with
  | Black -> White
  | White -> Black


let contain_leaf (tree_list: move_tree list) : bool =
  if tree_list = [] then true
  else (List.mem Leaf tree_list)



let rec get_min_max (tree:move_tree) (depth: int) (team:team) : float =
  match tree with
  | Node ((score, game, _) , tree_list)->
            if contain_leaf (tree_list) || (depth=0) then score else

            (match team with
                  |Black -> min_2 (List.map (fun a -> get_min_max a (depth-1)  (get_other_team team)) tree_list)
                  |White-> max_2 (List.map (fun b -> get_min_max b (depth-1) (get_other_team team)) tree_list)
            )

  |Leaf -> failwith "Never reaches"



let minimax (tree:move_tree) (depth: int) (team: team) : (float * move) =
  match tree with
  |Node ((_, game, _) , tree_list) -> min_1 (List.map (fun a ->

      match a with
            | Leaf -> failwith "There is no actual next layer"
            | Node ((_, _, move_opt) , _) ->
                      match move_opt with
                      | None -> failwith "There should be a move to get here"
                      | Some move ->  ((get_min_max a (depth-1) (get_other_team team)) , move)

       )
       tree_list )

  | Leaf -> failwith "There should be a top node"



let min_max (tree: move_tree) (levels: int) (team: team): (float * move option) =
  let x = minimax tree levels team in
  let (score, move) = x in
  (score, Some move)