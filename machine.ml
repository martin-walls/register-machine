open Instructions

(* represent register contents as a list of integers *)
(* let rec get_reg regs x = *)
(*   if x < 0 then 0 *)
(*   else *)
(*   match regs with *)
(*   | [] -> 0 *)
(*   | r::rs -> *)
(*       if x == 0 then *)
(*         r *)
(*       else *)
(*         get_reg (x-1) rs *)

(* let rec set_reg regs x v = *)
(*   if x < 0 then regs *)
(*   else *)
(*     match regs with *)
(*     | [] -> regs *)
(*     | r::rs -> *)
(*         if x == 0 then *)
(*           v::rs *)
(*         else *)
(*           r :: (set_reg rs (x-1) v) *)

let print_regs regs =
  let rec aux i len =
    if i < len then begin
      Printf.printf "r%d=%d" i regs.(i);
      if i < len - 1 then
        Printf.printf ", ";
      aux (i+1) len
    end
    else ()
  in
  Printf.printf "(";
  aux 0 (Array.length regs);
  Printf.printf ")\n"

let rec get_instr instrs n =
  if n < 0 then Halt
  else
  match instrs with
  | [] -> Halt
  | i::is ->
      if n == 0 then i
      else
        get_instr is (n-1)

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
      print_regs regs;
      exit 0
  | Nop -> ()

let run rs instrs =
  let l = ref 0 in
  let rec next_instr () =
    let i = get_instr instrs !l in
    run_instr i rs l;
    next_instr ()
  in
  next_instr ()

(* let () = Util.print_instr_list (Parse.parse "test.reg") *)

let run_file file =
  let (rs,is) = Parse.parse file in
  Printf.printf "Initial config:\t";
  print_regs rs;
  run rs is

let () =
  run_file "test.reg"
