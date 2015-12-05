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


let prompt_char = " > "
let move_tree_depth = 3
let move_tree_branch_factor = 0.1

let refresh () =
  for i=0 to 100 do print_endline ""; done

let print_move_instructions () =
  let _ = print_endline("--- INSTRUCTIONS ---") in
  let _ = print_endline("Moves are entered in the following format:") in
  let _ = print_endline("<source_column><source_row> to <dest_column><dest_row>") in
  let _ = print_endline("for example, 'a2 to a3' ") in
  print_endline("type 'quit' to exit\n")

let invalid_move_msg () =
  print_endline("Invalid Move.")

let square_empty (pos:boardpos) (b:board) : bool =
    let (bpos,pc') = !(get_square_on_board pos b) in
    match pc' with
      | Some _ -> false
      | None -> true

let rec game_input g name =
  let _ = print_string prompt_char in
  let cmd_type = parse_cmd(read_line()) in
    match cmd_type with
      | Quit -> exit 0
      | Move(src,dst) ->
        if square_empty src g.board then let _ = invalid_move_msg() in let _ = print_string(name^"s turn: Enter move") in game_input g name
        else
          let pce = get_piece g.board src in
          let move = (pce,src,dst) in
          begin
            match (valid_move move g) with
             |Valid mt -> move
             |Invalid _ -> let _ = invalid_move_msg() in
                           let _ = print_string(name^"'s turn: Enter move") in game_input g name
          end
      | _ -> print_endline("Please enter a valid command."); game_input g name


let rec game_loop g gt team pb_bool white_name black_name =
  let _ = if pb_bool then print_board(g.board) else () in
  let pmove =
    if (gt || team=White) then
      if(team = White) then let _ = print_string(white_name^"'s turn. Enter move") in game_input g white_name
      else let _ = print_string(black_name^"'s turn. Enter move") in game_input g black_name
  else
    (* Request move from the AI *)
    let _ = print_endline "AI pondering move..." in
    request_move 1 g team in
  (*let _ = print_endline "" in*)
  match pmove with
    | (piece, _, _) -> if piece.team <> team
  then let _ =  invalid_move_msg() in game_loop g gt team false white_name black_name else
  if (team = White) then
  (match (valid_move pmove g) with
    | Invalid ft ->  invalid_move_msg(); game_loop g gt White false white_name black_name
    | Valid mt -> print_endline(""); game_loop (update_game_with_move mt pmove g) gt Black true white_name black_name)
  else if gt
  then
    match (valid_move pmove g) with
      | Invalid ft ->   invalid_move_msg(); game_loop g gt Black false white_name black_name
      | Valid mt -> print_endline("");  game_loop (update_game_with_move mt pmove g) gt White true white_name black_name
  else
    match (valid_move pmove g) with
      | Invalid _ -> failwith "error"
      | Valid mt -> print_endline("");game_loop (update_game_with_move mt pmove g) gt White true white_name black_name

let rec main_input () =
  let _ = print_string prompt_char in
  let i = parse_cmd(read_line()) in
    match i with
      | StartPvP -> print_endline("");
                    print_endline("Starting PvP Game");
                    print_endline("");
                    print_string("Enter Player 1's name > ");
                    let white_name = read_line() in
                    print_string("Enter Player 2's name > ");
                    let black_name = read_line() in
                    print_move_instructions();
                    game_loop (make_game()) (true) White true white_name black_name
      | StartPvAI -> print_endline("");
                     print_endline("Starting PvAI Game");
                     print_endline("");
                     print_string("Enter your name > ");
                     let white_name = read_line() in
                     print_move_instructions();
                     game_loop (make_game()) (false) White true white_name "AI"
      | Quit -> exit 0
      | Move(a,b) -> print_endline("Please enter a valid command");
                     main_input()
      | InvalidCmd -> print_endline("Please enter a valid command");
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
