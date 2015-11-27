


(*Generates a string representation of a board from *)
let rec display b =
  let pks = display_helper_1 b in
  let strs = display_helper_2 pks in
  match strs with
    | [] -> ()
    | h::t -> match h with
                | [] -> print_endline(" a ")
                | h::t -> print_string(h)