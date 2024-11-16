open Imp
open Nimp

(* sort by ascending lower bound, and sort equals by ascending upper bound *)
let sort_2 l =
  List.stable_sort (fun (_, (l1, _)) (_, (l2, _)) -> l1 - l2) l
let sort_3 l =
  List.stable_sort (fun (_, (_, h1)) (_, (_, h2)) -> h1 - h2) l
let sort_intervals l =
  sort_2 (sort_3 l)

(* insert interval [i] in active list [l] 
   pre/post-condition: sorted by ascending upper bound *)
let rec insert_active i l =
  match l with 
  | [] -> [i]
  | (x,(lower,upper)):: rest ->
    let (_,(_,u)) = i in
      if u <= upper then i::l
      else
        (x,(lower,upper)) :: insert_active i rest

(* raw allocation information for a variable *)
type raw_alloc =
  | RegN  of int  (* index of the register *)
  | Spill of int  (* index of the spill *)

(* allocation of the local variables of a function [fdef] using linear scan
   algorithm, with [nb_regs] registers available for allocation ;
   return a raw allocation for each variable, as well as the maximum index of
   used registers, and the number of used stack slots (spills) *)
let lscan_alloc nb_regs fdef =
  let live_intervals = Liveness.liveness_intervals_from_liveness fdef in
  
  List.iter (fun (var, (start, end_)) ->
    Printf.printf "Variable: %s, Live Interval: [%d, %d]\n" var start end_;
  ) live_intervals;
   
  
  let alloc = Hashtbl.create (List.length fdef.locals) in
  let active = ref [] in
  let free = ref (List.init nb_regs (fun i -> i)) in
  let r_max = ref (-1) in (* maximum index of used register *)
  let spill_count = ref 0 in (* number of spilled variables *)
  (* free registers allocated to intervals that stop before timestamp a,
     returns remaining intervals *)
  let rec expire a l =
    match l with 
    | [] -> []
    | (x, (lower, upper))::rest ->
        if upper < a then
          ((match Hashtbl.find_opt alloc x with
            | Some (RegN reg) -> free := reg :: !free
            | _ -> ()
          );
          expire a rest)
        else (x, (lower, upper)):: expire a rest
      
  in
  (* for each interval i, in sorted order *)
  List.iter (fun i ->
      let xi, (li, hi) = i in
      (* free registers that expire before the lower bound of i *)
      active := expire li !active;
      (* if there are available registers *)
        (* ... then allocate one *)
        (* otherwise, may replace an already used register if this can
           make this register available again earlier *)
      if !free <> [] then
        let reg = List.hd !free in
        free := List.tl !free;
        Hashtbl.add alloc xi (RegN reg);
        r_max := max !r_max reg;
        active := insert_active i !active;
      else
        let latest = List.hd(List.rev !active) in
        let (_,(_,hlatest)) = latest in
        if hlatest > hi then
          let xlatest, _ = latest in 
          active:=List.rev (List.tl (List.rev !active));
          Hashtbl.add alloc xlatest (Spill !spill_count);
          spill_count := !spill_count + 1;
          let reg = Hashtbl.find alloc xlatest in
          Hashtbl.add alloc xi reg;
          active := insert_active i !active;
        else
          Hashtbl.add alloc xi (Spill !spill_count);
          spill_count := !spill_count + 1;
    ) (sort_intervals live_intervals);
  alloc, !r_max, !spill_count
