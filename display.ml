open Chesstypes
(*Unused*)

(*

	Chess Unicode Characters (might need help from a 3rd party lib?)
	- https://github.com/ytomino/unicode-ocaml
*)

let white_pcs = [("King","♔");("Queen","♕");("Bishop","♗");("Rook","♖");("Knight","♘");("Pawn","♙")]
let black_pcs = [("King","♚");("Queen","♛");("Bishop","♝");("Rook","♜");("Knight","♞");("Pawn","♟")]

let unicode_pieces = [
	(Black,black_pcs);
	(White,white_pcs)
]

let get_unicode (pce: piece) =
	let team_pcs = List.assoc pce.team unicode_pieces in
	List.assoc pce.name team_pcs

let print_sq_piece sq =
	let (pos,p) = !sq in
	match p with
		| Some pce -> print_string (" "^(get_unicode pce)^" ")
		| None  -> print_string " - "

let print_boardrow (r:row) (row_id:string) =
	let sqs = snd (List.split r) in
	let _ = print_string ("  "^row_id^"║") in
	let _ = List.iteri (fun i -> print_sq_piece) sqs in
	print_endline ""

let print_board (b:board) =
	let _ = List.iter (fun (id,r) -> (print_boardrow r id)) (List.rev b) in
	let _ = Printf.printf "   ╚═══════════════════════\n" in
	Printf.printf "     A  B  C  D  E  F  G  H\n"