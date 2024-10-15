open Imp
open Nimp

module VSet = Set.Make(String)
module VMap = Map.Make(String)

(* returns the set of variables accessed by the expression [e] *)
let rec use_expr e = match e with
  | Cst _  -> VSet.empty
  | Bool _ -> VSet.empty
  | Var x  -> VSet.singleton x
  | Binop (op, e1, e2) -> VSet.union (use_expr e1) (use_expr e2)
  | Call(_,args) -> List.fold_left (VSet.union) VSet.empty (List.map use_expr args)

let liveness fdef =
  let n = max_instr_list fdef.code in
  let live = Array.make (n+1) VSet.empty in
  (* returns the set of variable that live in entry to the numbered 
     instruction [i], assuming a set of live variables [lv_out] on 
     exit of [i] *)
  let rec lv_in_instr i lv_out = match i.instr with
    (* by case on the contents of i.instr *)
    | Putchar(e)  -> let s = VSet.union lv_out (use_expr e) in
                     live.(i.nb) <- s;
                     s
    | Set(x,e)    -> let s  = VSet.union lv_out (use_expr e) in
                     let s' = VSet.diff s (VSet.singleton x) in
                     live.(i.nb) <- s';
                     s'
    | If(e,seq1,seq2) -> 
        let s1 = lv_in_list seq1 lv_out in
        let s2 = lv_in_list seq2 lv_out in
        let s = s1 |> VSet.union s2 |> VSet.union (use_expr e) in
        live.(i.nb) <- s;
        s
    
    (* While ? two rounds are enough *)
    | While(e, seq) ->
        let lv_out' = lv_in_list seq lv_out |> VSet.union (use_expr e) in
        let s = lv_in_list seq lv_out' in
        let s' = s |> VSet.union (use_expr e) in
        live.(i.nb) <- s';
        s'
    | Expr(e)  -> let s = VSet.union lv_out (use_expr e) in
        live.(i.nb) <- s;
        s
    
    | Return(e)  -> let s = VSet.union lv_out (use_expr e) in
        live.(i.nb) <- s;
        s
                                    


  (* the same for a sequence, and records in [live] the live sets computed
     on entry to each analyzed instruction *)
  and lv_in_list l lv_out = match l with
    | [] -> lv_out
    | i :: l' ->
        let lv_out_i = lv_in_list l' lv_out in
        lv_in_instr i lv_out_i
  in
  let _ = lv_in_list fdef.code VSet.empty in
  live

let liveness_intervals_from_liveness fdef =
  let live = liveness fdef in
  (* for each variable [x], create the smallest interval that contains all
     the numbers of instructions where [x] is live *)
  let intervals = ref VMap.empty in
  let add_interval i var intervals =
    try
      let (start, finish) = VMap.find var intervals in
      VMap.add var (min start i, max finish i) intervals
    with Not_found ->
      VMap.add var (i, i) intervals
    in
  for i = 0 to Array.length live - 1 do
    VSet.iter (fun var ->
      intervals := add_interval i var !intervals
    ) live.(i)
  done;
    VMap.bindings !intervals

