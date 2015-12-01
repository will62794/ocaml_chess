open Chesstypes

type command =
  | Start
  | Quit
  | Move of boardpos * boardpos
  | InvalidCmd

let parse_move s =
  let cmd = String.lowercase s in
  if cmd = "start" then Start else
  if cmd = "quit" || cmd = "exit" then Quit else
  if String.length s <> 8
  then InvalidCmd
  else
    let p1r = String.sub cmd 1 1 in
    let p1c = String.sub cmd 0 1 in
    let p2r = String.sub cmd 7 1 in
    let p2c = String.sub cmd 6 1 in

    if p1c < "a" || p1c > "h" || p2c < "a" || p2c > "h" ||
       p1r < "0" || p1r > "8" || p2r < "0" || p2r > "8"
    then InvalidCmd
  else Move ((p1r,p1c),(p2r,p2c))