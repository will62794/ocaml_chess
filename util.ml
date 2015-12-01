open Chesstypes
open Chessmodel

let print_piece (p:piece) = 
	let team_str = match p.team with
		| White -> "White"
		| Black -> "Black" in
	Printf.printf "(%s,%s,%s)" p.name p.id team_str

let print_boardpos brdpos = 
	let (r,c) = brdpos in 
	(Printf.printf "(%s,%s) " r c)