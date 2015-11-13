(* 
	cmdparse.mli

	cmparse.ml parses textual input commands from the user interface
	and determines what action should be taken with respect to the current game

*)

(* edit this interface *)

(*Prints the board to the display*)
val print_board board -> () 

(*Takes in string from display*)
val parse_move string->move option 


