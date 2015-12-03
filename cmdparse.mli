open Chesstypes
(*
	cmparse.ml parses textual input commands from the user interface
	and determines what action should be taken with respect to the current game
*)


type command =
  | StartPvP
  | StartPvAI
  | Quit
  | Move of boardpos * boardpos
  | InvalidCmd

val parse_cmd : string -> command
