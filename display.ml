(*Unused*)

(* 

	Chess Unicode Characters (might need help from a 3rd party lib?) 
	- https://github.com/ytomino/unicode-ocaml 
*)

let white_pcs = [("King","♔");("Queen","♕");("Bishop","♗");("Rook","♖");("Knight","♘");("Pawn","♙")]
let black_pcs = [("King","♚");("Queen","♛");("Bishop","♝");("Rook","♜");("Knight","♞");("Pawn","♟")]

let unicode_pieces = [
	("Black",black_pcs);
	("White",white_pcs)
]