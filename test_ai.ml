open Chesstypes
open Chessmodel
open Rules
open Demoboards
open Util
open Game
open Move_tree
open Minmax


TEST_MODULE "simple ai move prediction" = struct

	let board_1 = make_init_board()
	let g = {make_empty_game() with board=board_1}

(* 	let st = Unix.gettimeofday() 
	let _ = print_endline "GENERATING TREE..." 
	let tree = generate_tree g 3 0.1 
	let _ = Printf.printf "GENERATED TREE in %f secs\n" (Unix.gettimeofday()-.st) 
	let chosen_move = 
	 match snd (min_max tree 2 White) with
		| None -> failwith ""
		| Some x -> x 

	let _ = print_move chosen_move *)

	let st = Unix.gettimeofday() 
	let pcs = all_team_pieces g.board White
	let mvs = List.map (fun pc -> possible_movements pc g) pcs
	let _ = Printf.printf "Found possible movements in %f secs\n" (Unix.gettimeofday()-.st) 



end