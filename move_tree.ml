
open Chesstypes
open Chessmodel
open Rules
open Eval
open Chessmoves
open Game
open Util




(*Float designates the value of the game as given by eval*)
type move_tree =
  | Leaf
  | Node of (float * game * move option) * move_tree list



(*This function takes in the current game and builds a move_tree whose nodes are
game states and whose children are game state after a move


type game = {
  board: board;
  players: player * player;
  total_moves: int;
  current_turn: team;
  in_enpassant: (piece option) list;
  did_castle: bool * bool; (* did_white_castle_yet, did_black_castle_yet *)
  moved_pieces: piece list;
}

*)
(*possible_movements: piece -> game -> (move * movetype) list*)



let get_all_possible_moves (team:team) (game:game) : (move*movetype) list=
  let team_pieces = get_all_pieces team game.board in
  (*Assumes that possible_movements exposed in rules.ml will return moves such
  as king move two spaces for EnPassant, plus correct trigger moves for Castling
  so forth...*)
  List.fold_left (fun a b -> List.append a (possible_movements b game)) [] team_pieces


(*val valid_move : move -> game -> move_validation*)

(*
let get_move_type (move:move) (game:game): movetype =
  let move_type = valid_move move game in
  match move_type with
  | Valid x-> x
  | Invalid y  -> failwith "Inconsistency. Possible movement from rules not
  validated by rules"
*)

let copy_game game =
  let brd_copy = copy_board game.board in
  let copy_game = {game with board=brd_copy}
  in copy_game


let rec get_first_n inc_to_dec_list n =
  if n=0 then [] else
  List.hd inc_to_dec_list :: get_first_n (List.tl inc_to_dec_list) (n-1)

(*Sorted, Sorted , Yes somewhat
Sorted, Rev_Sorted No
Rev_Sorted, Sorted No
Rev_Sortedd, Rev_Sorted*)



let filter_list (thelist: (move* movetype* float) list) (percentage:float) (team:team)=
match  team with
| White ->
let number_elements =  int_of_float (ceil (float_of_int((List.length thelist)) *. percentage)) in
let sorted_list = List.sort (fun a b ->
let (_,_,x) = a in let (_,_,y) = b in if x=y then 0 else if x>y then 1 else -1) thelist in
let rev_sorted_list = List.rev sorted_list in
get_first_n rev_sorted_list number_elements

| Black->
  let number_elements =  int_of_float (ceil (float_of_int((List.length thelist)) *. percentage)) in
  let sorted_list = List.sort (fun a b ->
  let (_,_,x) = a in let (_,_,y) = b in if x=y then 0 else if x>y then 1 else -1) thelist in
  (*let rev_sorted_list = List.rev sorted_list in *)
  get_first_n sorted_list number_elements


(*Might have bug with branching factor*)


let filter_failures (thelist: (move*movetype*(float option) ) list ) : (move* movetype * float ) list =
(*('a -> 'b -> 'a) -> 'a -> 'b list -> 'a*)
  List.fold_left (fun a b ->  match b with
  | (_,_, None) -> a
  | (x,y,Some z) -> List.append a [(x,y,z)]
  ) [] thelist


let eval_helper (game:game) (move:move) : float option =
  try Some (eval (copy_game game) move) with
  |_ -> None


let rec generate_tree_helper (prev_num: float) (game:game) (levels:int) (move_before:move) (branching_factor:float) : move_tree=
  if levels=0 then Leaf else
  let all_possible_moves = get_all_possible_moves ((copy_game game).current_turn) (game) in
  let assoc_list_3 = List.map (fun a -> (fst a , snd a , (eval_helper (copy_game game) (fst a) ))) all_possible_moves in
  let assoc_list = filter_failures assoc_list_3 in

  (*[(move, move_type, float option returned from eval)]*)
  (*update_game_with_move: move_type -> move -> game-> game*)
  let assoc_list_2 =  filter_list assoc_list branching_factor (copy_game game).current_turn in
  let produce_game_float t = match t with
  | (move, move_typ, score) -> ((update_game_with_move move_typ move (copy_game game)) , score, move)


in
  let game_float_move = List.map produce_game_float assoc_list_2 in
  (*[(game , float, move)]*)
  let move_tree_list = List.map (fun a ->
    match a with
    | (new_game, new_score, move_2) -> generate_tree_helper (prev_num +. new_score) (copy_game new_game) (levels -1) (move_2) (branching_factor))
     game_float_move in
  Node ((prev_num, (copy_game game), Some move_before) , move_tree_list)




(*Levels corresponds to how many steps down the tree it will go before
terminating with Leaf*)
let generate_tree (game:game) (levels:int) (branching_factor:float) =
let all_possible_moves = get_all_possible_moves ((copy_game game).current_turn) (game) in
  let assoc_list_3 = List.map (fun a -> (fst a , snd a , (eval_helper (copy_game game) (fst a) ))) all_possible_moves in
  let assoc_list = filter_failures assoc_list_3 in
  (*[(move, move_type, float option returned from eval)]*)
  (*update_game_with_move: move_type -> move -> game-> game*)

 let assoc_list_2 =  filter_list assoc_list branching_factor (copy_game game).current_turn in
  let produce_game_float t = match t with
  | (move, move_typ, score) -> ((update_game_with_move move_typ move (copy_game game)) , score, move) in
  let (game_float_move: (game*float*move) list) = List.map produce_game_float assoc_list_2 in
  (*[(game , float, move)]*)
  let move_tree_list = List.map (fun a ->
    match a with
    | (new_game, new_score, move_2) -> generate_tree_helper (0.0+.new_score) (copy_game new_game) (levels -1) (move_2)(branching_factor))
     game_float_move in
  Node ((0.0, (copy_game game), None) , move_tree_list)
  (*[(move, move_type)]*)
  (*update_game_with_move: move_type -> move -> game-> game*)


let rec print_tree (move_tree: move_tree): unit =
  match move_tree with
  | Node ((score, game , move_option) , move_tree_list) ->
      begin
      match move_option with
      |None -> let _ = print_string "This is the top Node " in
      let _ = print_string "The score for this board is  " in
      let _ = print_string (string_of_int (int_of_float score)) in
      let _ = print_string "\n" in
      let _ = print_game game in
      let _ = List.map (fun a -> print_tree a) move_tree_list in
      ()

      | Some move ->
      let (piece, (x,y), (x1, y1)) = move in
      let _ = print_string "This is board is given by" in
      let _ = print_string x ; print_string y in
      let _ = print_string "to " in
      let _= print_string x1 ; print_string y1 in
      let _ = print_piece piece in
      let _ = print_string "The score for this board is " in
      let _ = print_string (string_of_int (int_of_float score)) in
      let _= print_string "\n" in
      let _= print_game game in
      let _ = List.map (fun a -> print_tree a) move_tree_list in
      ()
      end
  | Leaf -> print_string "This is a leaf \n "
(*
let _ =
let init_game = make_game () in
let thetree = generate_tree init_game 2 0.5 in
let _= print_tree thetree in
()
*)

