open Game
open Rules
open Chessmoves
open Chesstypes
open Chessmodel
open Chessintel
open Cmdparse
open Display
open Move_tree
open Minmax

let intro =   "
 █████╗  █████╗ █████╗ ███╗   ███╗██╗     █████╗██╗  ██╗███████╗███████╗███████╗
██╔══██╗██╔═══╝██╔══██╗████╗ ████║██║    ██╔═══╝██║  ██║██╔════╝██╔════╝██╔════╝
██║  ██║██║    ███████║██╔████╔██║██║    ██║    ███████║█████╗  ███████╗███████╗
██║  ██║██║    ██╔══██║██║╚██╔╝██║██║    ██║    ██╔══██║██╔══╝  ╚════██║╚════██║
╚█████╔╝╚█████╗██║  ██║██║ ╚═╝ ██║██████╗╚█████╗██║  ██║███████╗███████║███████║
 ╚════╝  ╚════╝╚═╝  ╚═╝╚═╝     ╚═╝══════╝ ╚════╝╚═╝  ╚═╝╚══════╝╚══════╝╚══════╝"


let prompt_char = "> "
let move_tree_depth = 3
let move_tree_branch_factor = 0.1

let refresh () =
  for i=0 to 100 do print_endline ""; done

let print_move_instructions () =
  let _ = print_endline("--- INSTRUCTIONS ---") in
  let _ = print_endline("Moves are entered in the following format:") in
  let _ = print_endline("<source_column><source_row> to <dest_column><dest_row>") in
  let _ = print_endline("for example, 'a5 to b7' ") in
  print_endline("type 'quit' to exit\n")

let invalid_move_msg () = 
  print_endline("Invalid Move.\n")

let square_empty (pos:boardpos) (b:board) : bool = 
    let (bpos,pc') = !(get_square_on_board pos b) in
    match pc' with
      | Some _ -> false
      | None -> true

let rec game_input g =
  let _ = print_string prompt_char in
  let cmd_type = parse_cmd(read_line()) in
    match cmd_type with
      | Quit -> exit 0
      | Move(src,dst) -> 
        if square_empty src g.board then let _ = invalid_move_msg() in game_input g
        else
          let pce = get_piece g.board src in
          let move = (pce,src,dst) in
          begin 
            match (valid_move move g) with
             |Valid mt -> move
             |Invalid _ -> let _ = invalid_move_msg() in game_input g
          end
      | _ -> print_endline("Please enter a valid command."); game_input g

let rec game_loop g gt team =
  print_board(g.board);
  let pmove = 
    if (gt || team=White) then (
      (if(team = White) then Printf.printf "White's turn. Enter move"
      else Printf.printf "Black's turn. Enter move");
      game_input g)
  else 
    (* Request move from the AI *)
    let _ = print_endline "AI pondering move..." in
    request_move 1 g team in 
  let _ = print_endline "" in
  match pmove with
    | (piece, _, _) -> if piece.team <> team
  then let _ =  invalid_move_msg() in game_loop g gt team else
  if (team = White) then
  (match (valid_move pmove g) with
    | Invalid ft ->  invalid_move_msg(); game_loop g gt White
    | Valid mt ->  game_loop (update_game_with_move mt pmove g) gt Black)
  else if gt
  then
    match (valid_move pmove g) with
      | Invalid ft ->   invalid_move_msg(); game_loop g gt Black
      | Valid mt ->   game_loop (update_game_with_move mt pmove g) gt White
  else
    match (valid_move pmove g) with
      | Invalid _ -> failwith "error"
      | Valid mt ->  game_loop (update_game_with_move mt pmove g) gt White

let rec main_input () =
  let _ = print_string prompt_char in
  let i = parse_cmd(read_line()) in
    match i with
      | StartPvP -> print_endline("");
                    print_endline("Starting PvP Game");
                    print_endline("");
                    print_move_instructions();
                    game_loop (make_game()) (true) White
      | StartPvAI -> print_endline("");
                     print_endline("Starting PvAI Game");
                     print_endline("");
                     print_move_instructions();
                     game_loop (make_game()) (false) White
      | Quit -> exit 0
      | Move(a,b) -> print_endline("please enter a valid command");
                     main_input()
      | InvalidCmd -> print_endline("please enter a valid command");
                      main_input()

let main () =
  print_endline intro;
  print_endline "";
  print_endline "Welcome to OCamlChess!";
  print_endline "";
  print_endline "Type 'start pvp' or 'start pvai' to begin a game";
  print_endline "Type 'quit' to exit";
  main_input()


let _ = main()
