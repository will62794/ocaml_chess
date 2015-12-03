(*This module handles taking in chessmoves types*)

open Chesstypes


let update_game_with_move (movetyp:movetype) (move:move) (game:game) : game =
  game