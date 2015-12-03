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

let print_sq_piece sq =
	let (pos,p) = !sq in
	match p with
		| Some pce ->
			if (String.length pce.id)=1 then print_string (pce.id^"  ")
			else print_string (pce.id^" ")
		| None  -> print_string "-- "

let print_boardrow (r:row) =
	let sqs = snd (List.split r) in
	let _ = List.iter (print_sq_piece) sqs in
	print_endline ""

let print_board (b:board) =
	let rows = snd (List.split b) in
	List.iter print_boardrow (List.rev rows)

let make_empty_game () =
	{
		board = make_empty_board();
		players = ("","");
		total_moves = 0;
		current_turn = White;
		in_enpassant = [];
		did_castle = (false,false);
		moved_pieces = [];
	}