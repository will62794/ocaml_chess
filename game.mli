open Chesstypes

(* return a basic game setup *)
val make_game: unit -> game

(* given a piece and a game, returns true if that piece has been already moved in the game, false otherwise *)
val piece_moved_yet: piece -> game -> bool