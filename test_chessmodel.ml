open Chesstypes
open Chessmodel


TEST_MODULE "reflect_board" = struct

	let b = make_init_board()
	let reflected_b = (reflect_board_coords b)
	let (pos,piece) = !(get_square_on_board ("2","a") reflected_b)
	let pawn_black = match piece with | None -> failwith "" | Some x -> x
	let (pos,piece) = !(get_square_on_board ("7","a") reflected_b)
	let pawn_white = match piece with | None -> failwith "" | Some x -> x

	TEST = pawn_black.team=Black
	TEST = pawn_white.team=White

end