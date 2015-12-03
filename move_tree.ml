(*Float designates the value of the game as given by eval*)
type move_tree =
  | Leaf
  | Node of (float * game) * game list



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


let get_all_possible_moves team board=
  let team_pieces = get_all_pieces team board in
  let (game:game) = create_game (team) (board) in
  (*Assumes that possible_movements exposed in rules.ml will return moves such
  as king move two spaces for EnPassant, plus correct trigger moves for Castling
  so forth...*)
  List.fold_left (fun a b -> List.append b (possible_movements a game))


(*val valid_move : move -> game -> move_validation*)
let get_move_type (move:move) (game:game): move_type =
  let move_type = valid_move move game in
  match  with
  | Valid x-> x
  | Invalid y  -> failwith "Inconsistency. Possible movement from rules not
  validated by rules"

let rec generate_tree_helper (prev_num: float) (game:game) (levels:int): move_tree=
  if levels=0 then Leaf else
  let all_possible_moves = get_all_possible_moves (game.current_turn) (game.board) in
  let assoc_list = List.map (fun a -> (a, (get_move_type a game) , (eval game a))) all_possible_moves in
  (*[(move, move_type, float returned from eval)]*)
  (*update_game_with_move: move_type -> move -> game-> game*)
  let produce_game_float t = match t with
  | (move, move_typ, score) -> (update_game_with_move move_typ move game) , score ) in
  let game_float = List.map produce_game_float assoc_list in
  (*[(game , float)]*)
  let move_tree_list = List.map (fun a -> generate_tree_helper (prev_num +. snd a) (fst a)) game_float in
  Node ((prev_num, game) , move_tree_list)




(*Levels corresponds to how many steps down the tree it will go before
terminating with Leaf*)
let generate_tree (game:game) (levels:int) =
let all_possible_moves = get_all_possible_moves (game.current_turn) (game.board) in
  let assoc_list = List.map (fun a -> (a, (get_move_type a game) , (eval game a))) all_possible_moves in
  (*[(move, move_type, float returned from eval)]*)
  (*update_game_with_move: move_type -> move -> game-> game*)
  let produce_game_float t = match t with
  | (move, move_typ, score) -> (update_game_with_move move_typ move game) , score ) in
  let game_float = List.map produce_game_float assoc_list in
  (*[(game , float)]*)
  let move_tree_list = List.map (fun a -> generate_tree_helper (0.0 +. snd a) (fst a)) game_float in
  Node (0.0, game) , move_tree_list)
  (*[(move, move_type)]*)
  (*update_game_with_move: move_type -> move -> game-> game*)




