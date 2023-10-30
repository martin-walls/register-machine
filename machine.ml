open Instructions
open Printf

(* TODO *)
let sort_regs regs = regs

let print_config l regs =
  Printf.printf "(";
  Printf.printf "l=%d, " l;
  let rec aux rs =
    match rs with
    | [] -> ()
    | [Reg(i,r)] -> Printf.printf "r%d=%d)\n" i r
    | Reg(i,r)::rs ->
        Printf.printf "r%d=%d, " i r;
        aux rs
  in
  aux (sort_regs regs)

(* let print_config l regs = *)
(*   Printf.printf "("; *)
(*   Printf.printf "l=%d, " l; *)
(*   let rec aux i len = *)
(*     if i < len then begin *)
(*       Printf.printf "r%d=%d" i regs.(i); *)
(*       if i < len - 1 then *)
(*         Printf.printf ", "; *)
(*       aux (i+1) len *)
(*     end *)
(*     else () *)
(*   in *)
(*   aux 0 (Array.length regs); *)
(*   Printf.printf ")\n" *)

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

let get_reg regs n =
  if n < 0 then begin
    Printf.printf "Invalid register access";
    exit(1)
  end
  else
    let rec aux rgs return_regs =
      match rgs with
      | [] -> (0, Reg(n,0)::return_regs)
      | r::rs ->
          let Reg(i,rv) = r in
          if i == n then
            (rv, rgs @ return_regs)
          else
            aux rs (r::return_regs)
    in
    aux regs []

(* returns regs *)
let inc_reg regs n =
  if n < 0 then begin
    Printf.printf "Invalid register access";
    exit(1)
  end
  else
    let rec aux rgs return_regs =
      match rgs with
      | [] -> Reg(n,1)::return_regs
      | r::rs ->
          let Reg(i,rv) = r in
          if i == n then
            rs @ (Reg(i,rv+1)::return_regs)
          else
            aux rs (r::return_regs)
    in
    aux regs []

(* returns (success, regs) *)
let dec_reg regs n =
  if n < 0 then begin
    Printf.printf "Invalid register access";
    exit(1)
  end
  else
    let rec aux rgs return_regs =
      match rgs with
      | [] -> (false, Reg(n,0)::return_regs)
      | r::rs ->
          let Reg(i,rv) = r in
          if i == n then
            if rv > 0 then
              (true, rs @ (Reg(i,rv-1)::return_regs))
            else
              (false, rgs @ return_regs)
          else
            aux rs (r::return_regs)
    in
    aux regs []

(* regs is a list of integers, the register contents *)
(* l is the current instruction label, an int *)
let run_instr i regs l =
  match i with
  | Add (r,l1) ->
      regs := inc_reg !regs r;
      (* regs.(r) <- regs.(r) + 1; *)
      l := l1
  | Sub (r,l1,l2) ->
      let (success,rs) = dec_reg !regs r in
      regs := rs;
      if success then begin
        (* regs.(r) <- regs.(r) - 1; *)
        l := l1
      end
      else
        l := l2
  | Halt ->
      Printf.printf "Final config:\t";
      print_config !l !regs;
      exit 0

let run l rs instrs =
  let l = ref l in
  let regs = ref rs in
  let rec next_instr () =
    let i = get_instr instrs !l in
    run_instr i regs l;
    next_instr ()
  in
  next_instr ()

let run_file file =
  let (l,regs,instrs) = Parse.parse file in
  Printf.printf "Initial config:\t";
  print_config l regs;
  run l regs instrs

let main () =
  let argv = Array.to_list Sys.argv in
  let args = List.tl argv in
  match args with
  | [] -> Printf.printf "Usage: machine.byte <source file>"
  | file::_ -> run_file file

let () = main ()
(* let () = List.iter (fun r -> let Reg(i,rv) = r in Printf.printf "%d=%d " i rv) *)
(*   (Parse.number_regs (1::2::3::[])) *)
  (* (Reg(0,1)::Reg(1,2)::Reg(3,4)::[]) *)

(* let () = let r,rs = get_reg (Reg(0,3)::Reg(3,7)::[]) 4 in *)
(*   Printf.printf "%d" r *)
