open Chesstypes
open Chessmodel

let make_game () =
	{
		board = make_init_board();
		players = ("p1","p2");
		total_moves = 0;
		current_turn = White;
		in_enpassant = None;
		did_castle = (false,false); (* did_white_castle_yet, did_black_castle_yet *)
		moved_pieces = [];
	}

let piece_moved_yet (p:piece) (g:game) =
	List.mem p g.moved_pieces






	(*

type game = {
  board: board;
  players: player * player;
  total_moves: int;
  current_turn: team;
  in_enpassant: piece option;
  did_castle: bool * bool; (* did_white_castle_yet, did_black_castle_yet *)
  moved_pieces: piece list;
}
*)
