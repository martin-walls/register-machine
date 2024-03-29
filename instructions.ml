(* ADD r,l *)
(* SUB r,l1,l2 *)
(* HALT *)

type instr_body = Add of int * int
                | Sub of int * int * int
                | Halt

type instr = Instr of int * instr_body
           | Nop

type reg = Reg of int * int

(* regex matching for instructions *)
let add_pattern = Str.regexp "^ *\\([0-9]+\\): *\\(ADD\\|add\\) +\\(r\\|R\\)?\\([0-9]+\\), *\\(l\\|L\\)?\\([0-9]+\\)"

let sub_pattern = Str.regexp "^ *\\([0-9]+\\): *\\(SUB\\|sub\\) +\\(r\\|R\\)?\\([0-9]+\\), *\\(l\\|L\\)?\\([0-9]+\\), *\\(l\\|L\\)?\\([0-9]+\\)"

let halt_pattern = Str.regexp "^ *\\([0-9]+\\): *\\(HALT\\|halt\\)"

let initial_config_pattern = Str.regexp "^ *( *\\(\\([0-9]+, *\\)*[0-9]+\\) *)"

let print_instr_body i =
  match i with
  | Add (r,l) -> Printf.printf "ADD r%d, l%d\n" r l
  | Sub (r,l1,l2) -> Printf.printf "SUB r%d, l%d, l%d\n" r l1 l2
  | Halt -> Printf.printf "Halt\n"

let print_instr i =
  match i with
  | Instr (i,bdy) ->
      Printf.printf "%d: " i;
      print_instr_body bdy
  | Nop -> Printf.printf "Nop\n"


let rec print_instr_list is =
  match is with
  | [] -> ()
  | i::is -> print_instr i; print_instr_list is
