open Instructions

(* initial config * instruction sequence *)
type program = int array * instr list

let initial_config_pattern = Str.regexp "^ *( *\\(\\([0-9]+, *\\)*[0-9]+\\) *)"
let int_pattern = Str.regexp "\\([0-9]+\\)"

let rec print_split ss =
  match ss with
  | [] -> ()
  | s::ss -> Printf.printf "%d " s;
             print_split ss


let parse_initial_config line =
  let s = Str.string_match initial_config_pattern line 0 in
  if s then
    let rs_string = Str.matched_group 1 line in
    let split = Str.split (Str.regexp ", *") rs_string in
    let rs = List.map (fun s -> int_of_string s) split in
    Array.of_list rs
  else begin
    Printf.printf "Invalid initial config: %s\n" line;
    exit 0
  end

let parse_line line =
  (* Printf.printf "parsing line %s\n" line; *)
  let s = Str.string_match add_pattern line 0 in
  if s then
    let reg = int_of_string (Str.matched_group 3 line) in
    let dst = int_of_string (Str.matched_group 5 line) in
    Add (reg, dst)
  else
    let s = Str.string_match sub_pattern line 0 in
    if s then
      let reg = int_of_string (Str.matched_group 3 line) in
      let dst1 = int_of_string (Str.matched_group 5 line) in
      let dst2 = int_of_string (Str.matched_group 7 line) in
      Sub (reg, dst1, dst2)
    else
      let s = Str.string_match halt_pattern line 0 in
      if s then
        Halt
      else
        Nop

let rec parse_lines ic instrs =
  try
    let line = input_line ic in
    let instr = parse_line line in
    parse_lines ic (instr :: instrs)
  with End_of_file ->
    instrs

let parse file =
  let ic = open_in file in
  let instrs = [] in
  try
    let rs = parse_initial_config (input_line ic) in
    let is = List.rev (parse_lines ic instrs) in
    close_in ic;
    (rs, is)
  with e ->
    Printf.printf "Error reading source file.";
    close_in_noerr ic;
    raise e


(* let main () = *)
(*   let argv = Array.to_list Sys.argv in *)
(*   let args = List.tl argv in *)
(*   match args with *)
(*   | [] -> Printf.printf "Hello, world!\n" *)
(*   | names -> Printf.printf "Hello, %s!\n" (join names) *)

