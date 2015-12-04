
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
		in_enpassant = None;
		did_castle = (false,false); (* did_white_castle_yet, did_black_castle_yet *)
		moved_pieces = [];
	}


let print_game (game:game)=
	let _ = print_board (game.board) in
	let _ = print_string "The number of turns " ; print_string (string_of_int game.total_moves) in
	let _ =
	if (game.current_turn = White) then print_string "White team's turn \n" else
	print_string "Black team's turn \n" in
	let _ = match game.in_enpassant with
	| None -> print_string "There is no peice subject to elpessant \n"
	| Some p-> print_string "There is a piece in enpassant \n" ; print_piece p
in
let _ = print_string "White has castled \n" ; print_string (string_of_bool (fst game.did_castle)) in
let _ =  print_string "Black has castled \n" ; print_string (string_of_bool (snd game.did_castle)) in
let _ = List.map (fun p -> print_piece p ; ()) game.moved_pieces in
()


