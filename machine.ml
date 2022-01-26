open Instructions

let print_config l regs =
  Printf.printf "(";
  Printf.printf "l=%d, " l;
  let rec aux i len =
    if i < len then begin
      Printf.printf "r%d=%d" i regs.(i);
      if i < len - 1 then
        Printf.printf ", ";
      aux (i+1) len
    end
    else ()
  in
  aux 0 (Array.length regs);
  Printf.printf ")\n"

let rec get_instr instrs n =
  if n < 0 then Halt
  else
  match instrs with
  | [] -> Halt
  | Instr(i,bdy)::is ->
      if n == i then bdy
      else
        get_instr is n
  | Nop::is -> get_instr is n

(* regs is an array of integers, the register contents *)
(* l is the current instruction label, an int *)
let run_instr i regs l =
  match i with
  | Add (r,l1) ->
      regs.(r) <- regs.(r) + 1;
      l := l1
  | Sub (r,l1,l2) ->
      if regs.(r) > 0 then begin
        regs.(r) <- regs.(r) - 1;
        l := l1
      end
      else
        l := l2
  | Halt ->
      Printf.printf "Final config:\t";
      print_config !l regs;
      exit 0

let run l rs instrs =
  let l = ref l in
  let rec next_instr () =
    let i = get_instr instrs !l in
    run_instr i rs l;
    next_instr ()
  in
  next_instr ()

let run_file file =
  let (l,rs,is) = Parse.parse file in
  Printf.printf "Initial config:\t";
  print_config l rs;
  run l rs is

let main () =
  let argv = Array.to_list Sys.argv in
  let args = List.tl argv in
  match args with
  | [] -> Printf.printf "Usage: machine.byte <source file>"
  | file::_ -> run_file file

let () = main ()
