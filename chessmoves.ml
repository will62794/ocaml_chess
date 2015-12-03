(*This module handles taking in chessmoves types*)

open Chesstypes
open Chessmodel
open Game



(*Given a movetype representing an action on the game board, update_game_with
move, returns a new copy of the game with all the appropiate pieces moved. Deep
copy. Updates the score

type game = {
  board: board;
  players: player * player;
  total_moves: int;
  current_turn: team;
  in_enpassant: (piece option) list;
  did_castle: bool * bool; (* did_white_castle_yet, did_black_castle_yet *)
  moved_pieces: piece list;
}

update_game_with_move returns a game with  a new game with all the appropiate
fields updated



type movetype = Basic | Capture | EnPassant | CastlingLeft | CastlingRight | PawnPromotion

*)


let get_other_team (team:team) =
  match team with
  | Black -> White
  | White -> Black


let get_moved_piece_list (piece: piece) (moved_piece_list: piece list) =
  (*Only one instance of piece in the list*)
  if (List.mem piece moved_piece_list) then moved_piece_list else
  List.append [piece] moved_piece_list

(*-------- Black --------
  a8 b8 c8 d8 e8 f8 g8 h8
  a7 b7 c7 d7 e7 f7 g7 h7
  a6 b6 c6 d6 e6 f6 g6 h6
  a5 b5 c5 d5 e5 f5 g5 h5
  a4 b4 c4 d4 e4 f4 g4 h4
  a3 b3 c3 d3 e3 f3 g3 h3
  a2 b2 c2 d2 e2 f2 g2 h2
  a1 b1 c1 d1 e1 f1 g1 h1
  -------- White --------

type boardpos = string * string (* (row,column) e.g. (5,a) *)

*)


let get_enpassant (move:move) : piece option =
  match move with
  | (p, board_pos1, board_pos2) -> if (p.piecetype!=Pawn) then None else
  if (int_of_string (fst (board_pos1)) = 2) &&
  (int_of_string (fst (board_pos1)) = 4)
  || (int_of_string (fst (board_pos1)) =7 ) &&
  ((int_of_string (fst board_pos1)) = 5) then Some p else None


let do_basic_move movetyp move game =
  let (p, board_pos1, board_pos2) = move in
  let new_board =
  match execute_move (move) (game.board) with
  | Some b  -> b
  | _ -> failwith "There should be a board"
  in
  (*Assumes execute_move_returns deep copy of the board*)
  {
  board=new_board ;
  players = game.players ;
  total_moves= game.total_moves+1 ;
  current_turn = get_other_team (game.current_turn) ;
  in_enpassant=get_enpassant (move) ;
  did_castle=game.did_castle ;
  moved_pieces= get_moved_piece_list p game.moved_pieces
  }


let do_enpassant_move move game =
  let (p, board_pos1, board_pos2) = move in
  let theboard =
  match  execute_move (move) (game.board) with
  | Some b -> b
  | _ -> failwith "There should be a board"
in
  (*Assumes execute_move returns a deep copy*)
  let (start_x, start_y) = boardpos_to_coords board_pos1 in
  let (end_x, end_y) = boardpos_to_coords board_pos2 in
  let target_square_ref =
  if (start_y < end_y) then get_square_on_board (coords_to_boardpos (end_x, (end_y-1))) theboard
  else get_square_on_board (coords_to_boardpos (end_x, (end_y+1))) theboard in
  (*boardpos * piece option*)
  let _ = target_square_ref:= (coords_to_boardpos (end_x, (end_y+1)) , None) in
  {
  board=theboard ;
  players = game.players ;
  total_moves= game.total_moves+1 ;
  current_turn = get_other_team (game.current_turn) ;
  in_enpassant=None ;
  did_castle=game.did_castle ;
  moved_pieces= get_moved_piece_list p game.moved_pieces
  }

(*castling left white
White King (1,e) to (1,c) and White Rook from (1,a) to (1,d)

castling right white
White King (1,e) to (1,g) and White Rook from (1,h) to (1,f)

castling left black
Black King (8,e) to (8,c) and Black Rook from (8,a) to (8,d)

castling right black
Black King (1,e) to (1,g) and Black Rook from (1,h) to (1,f)*)



let do_castle_left move game =
  let current_team = game.current_turn in
  match current_team with
  | White ->
  let king_piece = get_piece game.board ("1" , "e") in
  let rook_piece = get_piece game.board ("1" , "a") in
  let _ = put_piece_at_boardpos (Some king_piece) ("1", "c") (game.board) in
  let _ = put_piece_at_boardpos (Some rook_piece) ("1" , "d") (game.board) in
  {
  board=game.board ;
  players = game.players ;
  total_moves= game.total_moves+1 ;
  current_turn = get_other_team (game.current_turn) ;
  in_enpassant=None ;
  did_castle=(true, snd (game.did_castle)) ;
  moved_pieces= let moved_pieces = get_moved_piece_list king_piece
  game.moved_pieces in get_moved_piece_list rook_piece moved_pieces
  }

  | Black ->
  let king_piece = get_piece game.board ("8" , "e") in
  let rook_piece = get_piece game.board ("8" , "a") in
  let _ = put_piece_at_boardpos (Some king_piece) ("8", "c") (game.board) in
  let _ = put_piece_at_boardpos (Some rook_piece) ("8" , "d") in
  {
  board=game.board ;
  players = game.players ;
  total_moves= game.total_moves+1 ;
  current_turn = get_other_team (game.current_turn) ;
  in_enpassant=None ;
  did_castle=(fst (game.did_castle),  true) ;
  moved_pieces= let moved_pieces = get_moved_piece_list king_piece
  game.moved_pieces in get_moved_piece_list rook_piece moved_pieces
  }

let do_castle_right move game =
  let current_team = game.current_turn in
  match current_team with
  | White ->
  let king_piece = get_piece (game.board) ("1" , "e") in
  let rook_piece = get_piece (game.board) ("1" , "h") in
  let _ = put_piece_at_boardpos (Some king_piece) ("1", "g") (game.board) in
  let _ = put_piece_at_boardpos (Some rook_piece) ("1" , "f") in
  {
  board=game.board ;
  players = game.players ;
  total_moves= game.total_moves+1 ;
  current_turn = get_other_team (game.current_turn) ;
  in_enpassant=None ;
  did_castle=(true, snd (game.did_castle)) ;
  moved_pieces= let moved_pieces = get_moved_piece_list king_piece
  game.moved_pieces in get_moved_piece_list rook_piece moved_pieces
  }

  | Black ->
  let king_piece = get_piece (game.board) ("8" , "e") in
  let rook_piece = get_piece (game.board) ("8" , "h") in
  let _ = put_piece_at_boardpos (Some king_piece) ("8", "g") (game.board) in
  let _ = put_piece_at_boardpos (Some rook_piece) ("8" , "f") in
  {
  board=game.board ;
  players = game.players ;
  total_moves= game.total_moves+1 ;
  current_turn = get_other_team (game.current_turn) ;
  in_enpassant=None ;
  did_castle=(fst (game.did_castle),  true) ;
  moved_pieces= let moved_pieces = get_moved_piece_list king_piece
  game.moved_pieces in get_moved_piece_list rook_piece moved_pieces
  }

let do_pawn_promotion (move:move) (game:game) =
  let (p, board_pos1, board_pos2) = move in
  let theboard =
  match execute_move (move) (game.board) with
  | Some x -> x
  |None -> failwith "There should be a baord"
in
  (*create_piece: string -> team -> string -> piecekind -> piece*)
  let random_num = Random.float 1000.0 in
  let new_id = string_of_int (int_of_float random_num) in
  let new_queen = create_piece (new_id) (game.current_turn) ("Queen") (Queen) in
  let _ = put_piece_at_boardpos (Some new_queen) (board_pos2) in
  {
  board=theboard ;
  players = game.players ;
  total_moves= game.total_moves+1 ;
  current_turn = get_other_team (game.current_turn) ;
  in_enpassant=None ;
  did_castle=game.did_castle ;
  moved_pieces= game.moved_pieces
  }

let update_game_with_move (movetyp:movetype) (move:move) (game:game) : game =
  let copy_game = deep_copy_game game in
  match movetyp with
  |Basic -> do_basic_move movetyp move copy_game
  |Capture-> do_basic_move movetyp move copy_game
  |EnPassant-> do_enpassant_move move copy_game
  |CastlingLeft-> do_castle_left move copy_game
  |CastlingRight-> do_castle_right move copy_game
  |PawnPromotion-> do_pawn_promotion move copy_game

(******************************************************************************



                                  Testsing



********************************************************************************)

let _ =
let init_game = make_game () in
let king_piece = get_piece init_game.board ("e" , "1")
let _ = display init_game.board in
let move = (king_piece, ("e" , "1") , ("g" , "1") ) in
let new_game = update_game_with_move (CastlingRight) (move) (init_game) in
display new_game.board


