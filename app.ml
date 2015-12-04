open Game
open Rules
open Chessmoves
open Chesstypes
open Chessmodel
open Chessintel
open Cmdparse
open Util


let rec game_input g =
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

let rec game_loop g gt =
  print_endline("Enter Player_1's Move");
  let p1move = game_input g in
  let g' =
    match (valid_move p1move g) with
      | Invalid ft -> failwith("invalid move")
      | Valid mt -> update_game_with_move mt p1move g
  in
  print_board(g'.board);
  if gt
  then
    let _ = print_endline("Enter Player_2's Move") in
    let p2move = game_input g' in
    let g'' = match (valid_move p2move g') with
              | Invalid ft -> failwith("invalid move")
              | Valid mt -> update_game_with_move mt p2move g'
    in
    let _  = print_board(g''.board) in
    game_loop (g'') (gt)
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
  let i = parse_cmd(read_line()) in
    match i with
      | StartPvP -> print_endline("");
                    print_endline("Starting PvP Game");
                    print_endline("");
                    print_endline("Enter Moves in the format:");
                    print_endline("<SourceRow><SourceColumn> to " ^
                                  "<DestinationRow><Column> " ^
                                  "or type Quit to exit");
                    game_loop (make_game()) (true)
      | StartPvAI -> print_endline("");
                     print_endline("Starting PvAI Game");
                     print_endline("");
                     print_endline("Enter Moves in the format:");
                     print_endline("<SourceRow><SourceColumn> to " ^
                                  "<DestinationRow><DestinationColumn> " ^
                                  "or type Quit to exit");
                     game_loop (make_game()) (false)
      | Quit -> exit 0
      | Move(a,b) -> print_endline("please enter a valid command");
                     main_input()
      | InvalidCmd -> print_endline("please enter a valid command");
                      main_input()

let main () =
  print_endline "";
  print_endline "Welcome to OCamlChess!";
  print_endline "";
  print_endline "Type Start PvP or Start PvAI to begin a game";
  print_endline "Type Quit to exit";
  main_input()


let _ = main()
