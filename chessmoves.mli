(*This module handles taking in chessmoves types*)

open Chesstypes



(*Given a movetype representing an action on the game board, update_game_with
move, returns a new copy of the game with all the appropiate pieces moved. Deep
copy. Updates the score

type game = {
  board: board;
  players: player * player;
  total_moves: int;
  current_turn: team;
  in_enpassant: (piece option) list;
  did_castle: bool * bool; (* did_white_castle_yet, did_black_castle_yet *)
  moved_pieces: piece list;
}

update_game_with_move returns a game with  a new game with all the appropiate
fields updated

*)


val update_game_with_move: movetype -> move -> game-> game
