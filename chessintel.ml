open Chesstypes
open Move_tree
open Minmax

let tree_depth = 3
let tree_branch_factor = 0.2

let request_move difficulty g team =
  let tree = generate_tree g tree_depth tree_branch_factor in
  match snd (min_max tree 2 team) with
        | None -> failwith "ai move request error"
        | Some m -> m
