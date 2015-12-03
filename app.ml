open Chesstypes
open Cmdparse
open Game


let rec game_input () =
  let i = parse_cmd(read_line()) in
    match i with
      | Quit -> (("0","0"),("0","0"))
      | Move(x,y) -> (x,y)
      | _ -> print_endline("please enter a valid command"); game_input()

let game_loop g gt =
  let (p1move_r,p1move_c) = game_input() in
  if (p1move_r,p1move_c) = (("0","0"),("0","0"))
  then ()
  else ()


let rec main_input () =
  let i = parse_cmd(read_line()) in
    match i with
      | StartPvP -> print_endline("");
                    print_endline("Starting PvP Game");
                    print_endline("");
                    print_endline("Enter Moves in the format:");
                    print_endline("<SourceRow><SourceColumn> to" ^
                                  "<DestinationRow><Column>" ^
                                  "or type Quit to exit");
                    game_loop (make_game()) (true)
      | StartPvAI -> print_endline("");
                     print_endline("Starting PvAI Game");
                     print_endline("");
                     print_endline("Enter Moves in the format:");
                     print_endline("<SourceRow><SourceColumn> to " ^
                                  "<DestinationRow><DestinationColumn> " ^
                                  "or type Quit to exit");
                     game_loop (make_game) (false)
      | Quit -> ()
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
