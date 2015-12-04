open Game
open Rules
open Chessmoves
open Chesstypes
open Chessmodel
open Chessintel
open Cmdparse
open Display

let prompt_char = "$ "

let refresh () = 
  for i=0 to 100 do print_endline ""; done

let print_move_instructions () = 
  let _ = print_endline("- INSTRUCTIONS -") in
  let _ = print_endline("Moves are entered in the following format:") in
  let _ = print_endline("<source_column:source_row> to <dest_column><dest_row>") in
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
                        | None -> print_endline("there's no piece there");
                                  game_input g
                        | Some pc -> (pc,x,y)
                      end
      | _ -> print_endline("please enter a valid command"); game_input g

let rec game_loop g gt white =
  refresh();
  print_board(g.board);
  if (white) then (
  print_endline("Enter Player 1's Move");
  let p1move = game_input g in
  match (valid_move p1move g) with
  | Invalid ft -> Printf.printf "INVALID MOVE\n"; game_loop g gt true
  | Valid mt -> game_loop (update_game_with_move mt p1move g) gt false)
  else if gt
  then
    let _ = print_endline("Enter Player 2's Move") in
    let p2move = game_input g in
    match (valid_move p2move g) with
    | Invalid ft -> Printf.printf "INVALID MOVE\n"; game_loop g gt false
    | Valid mt -> game_loop (update_game_with_move mt p2move g) gt true
  else
    failwith ("unimplemented")
    (*let aimove = request_move 1 g'.board in
    let g'' =
    match (valid_move aimove g') with
      | Invalid ft -> failwith("this AI sucks")
      | Valid mt -> update_game_with_move mt aimove g'
    in
    let _ = print_board (g''.board) in
    game_loop (g'') (gt) *)

let rec main_input () =
  let _ = print_string prompt_char in
  let i = parse_cmd(read_line()) in
    match i with
      | StartPvP -> print_endline("");
                    print_endline("Starting PvP Game");
                    print_endline("");
                    print_move_instructions();
                    game_loop (make_game()) (true) true
      | StartPvAI -> print_endline("");
                     print_endline("Starting PvAI Game");
                     print_endline("");
                     print_move_instructions();
                     game_loop (make_game()) (false) true
      | Quit -> exit 0
      | Move(a,b) -> print_endline("please enter a valid command");
                     main_input()
      | InvalidCmd -> print_endline("please enter a valid command");
                      main_input()

let main () =
  print_endline "";
  print_endline "Welcome to OCamlChess!";
  print_endline "";
  print_endline "Type 'start pvp' or 'start pvai' to begin a game";
  print_endline "Type 'quit' to exit";
  main_input()


let _ = main()
