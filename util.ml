
open Chesstypes
open Chessmodel
open Game
open Display

let white_pcs = [("King","♕");("Queen","♔");("Bishop","♗");("Rook","♖");("Knight","♘");("Pawn","♙")]
let black_pcs = [("King","♚");("Queen","♛");("Bishop","♝");("Rook","♜");("Knight","♞");("Pawn","♟")]

let unicode_pieces = [
	(Black,black_pcs);
	(White,white_pcs)
]

let get_unicode (pce: piece) =
	let team = pce.team in
	let team_pcs = List.assoc team unicode_pieces in
	List.assoc pce.name team_pcs

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
		| Some pce -> print_string (" "^(get_unicode pce)^" ")
		| None  -> print_string " - "

let print_boardrow (r:row) (n: int)   =
	let (rws,sqs) = (List.split r) in
	let _ = Printf.printf "%d║" n in
	let _ = List.iter (print_sq_piece) sqs in
	print_endline ""

let rec print_board_help (rows:row list) (n: int) =
	match rows with
	| [] -> ()
	| h::t -> print_boardrow h n; print_board_help t (n-1)

let print_board (b:board) =
	print_board_help (snd (List.split b)) 8;
	Printf.printf " ╚═══════════════════════\n";
	Printf.printf "   A  B  C  D  E  F  G  H\n"

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


