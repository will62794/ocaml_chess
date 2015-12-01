open Chesstypes
open Chessmodel

let make_game () = 
	{
		board = make_init_board();
		players = ("p1","p2");
		total_moves = 0;
		current_turn = White;
		in_enpassant = [];
		did_castle = (false,false); (* did_white_castle_yet, did_black_castle_yet *)
	}