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

let refresh () =
  for i=0 to 100 do print_endline ""; done

let print_move_instructions () =
  let _ = print_endline("- INSTRUCTIONS -") in
  let _ = print_endline("Moves are entered in the following format:") in
  let _ = print_endline("<source_column><source_row> to <dest_column><dest_row>") in
  let _ = print_endline("for example, 'a5 to b7' ") in
  print_endline("type 'quit' to exit\n")


let rec game_input g =
  let _ = print_string prompt_char in
  let i = parse_cmd(read_line()) in
    match i with
      | Quit -> exit 0
      | Move(x,y) -> let (bpos,pc') = !(get_square_on_board x g.board) in
                      begin
                       match pc' with
                        | None -> print_endline("INVALID MOVE\n");
                                  game_input g
                        | Some pc -> (pc,x,y)
                      end
      | _ -> print_endline("please enter a valid command"); game_input g

let rec game_loop g gt team =
  print_board(g.board);
  let pmove = if (gt || team=White) then ((if(team = White) then Printf.printf "Enter white's move"
  else Printf.printf "Enter black's move");
  game_input g)
  else
      (let tree = generate_tree g 4 0.1 in
      let _ = print_tree tree in
      (match snd (min_max tree 2 team) with
                  | None -> failwith ""
                  | Some x -> x)) in
  match pmove with
  | (piece, _, _) -> if piece.team <> team
  then let _ = refresh(); Printf.printf "INVALID MOVE\n" in game_loop g gt team else
  if (team = White) then
  (match (valid_move pmove g) with
  | Invalid ft ->   refresh();Printf.printf "INVALID MOVE\n"; game_loop g gt White
  | Valid mt ->   refresh(); game_loop (update_game_with_move mt pmove g) gt Black)
  else if gt
  then
    match (valid_move pmove g) with
    | Invalid ft ->   refresh();Printf.printf "INVALID MOVE\n"; game_loop g gt Black
    | Valid mt ->   refresh(); game_loop (update_game_with_move mt pmove g) gt White
  else
    match (valid_move pmove g) with
    | Invalid _ -> failwith "err0r"
    | Valid mt -> refresh(); game_loop (update_game_with_move mt pmove g) gt White

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
