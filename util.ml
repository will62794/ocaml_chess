open Chesstypes
open Chessmodel
open Game


let print_piece (p:piece) = 
	let team_str = match p.team with
		| White -> "White"
		| Black -> "Black" in
	Printf.printf "(%s,%s,%s)" p.name p.id team_str

let print_boardpos brdpos = 
	let (r,c) = brdpos in 
	(Printf.printf "(%s,%s) " r c)

let make_empty_game () = 
	{
		board = make_empty_board();
		players = ("","");
		total_moves = 0;
		current_turn = White;
		in_enpassant = [];
		did_castle = (false,false); (* did_white_castle_yet, did_black_castle_yet *)
	}