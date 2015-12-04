open Move_tree
open Eval
open Game
open Chesstypes
open Minmax

let game = make_game () in
let tree = generate_tree game 2 in
let x = min_max tree 2 White in
Printf.printf "POINT VALUE OF BEST POSSIBLE MOVE: %f\n" (fst x);
match snd x with
| Some (piece, (x, y), (x', y')) -> Printf.printf "BEST POSSIBLE MOVE: %s%s to %s%s\n" x y x' y'
| _ -> failwith ""