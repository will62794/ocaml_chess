(* 
	graphics.mli

	graphics.ml handles the production of good looking ASCII output given some 
	part of the internal data model of the game (board, player, etc.)

*)

(* edit this interface *)

(*Takes in a board and returns a string that is a graphical representation of the board nicely formatted*) 
val board_display_string board->string 
