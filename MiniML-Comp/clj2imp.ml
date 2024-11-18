(**
   Translation from Clj to Imp

   To avoid name clashes, the translation generates fresh names on the fly 
   for every variable.
 *)

(* Module for variables renamings *)
module STbl = Map.Make(String)

(* Translation of variables
   - named variables are looked up in the renaming table (or accessed
     directly)
   - closure variables generate an array access *)
let tr_var v env = match v with
  | Clj.Name(x) ->
    Imp.(if STbl.mem x env then Var(STbl.find x env) else Var x)
      
  | Clj.CVar(n) ->
    Imp.(array_get (Var "closure") (Int n))
      
(* Translation of an expression

   Parameters:
   - e   : Clj expression
   - env : variable renamings

   Result: triple (s, e', vars)
   - s    : Imp instruction sequence
   - e'   : Imp expression
   - vars : list of fresh variable names introduced for renamings
   
   Spec: executing s then evaluating e' in IMP is equivalent to evaluating e in CLJ
*)
let tr_expr e env =
  (* Counter for fresh variable names *)
  let cpt = ref (-1) in
  (* List of generated names *)
  let vars = ref [] in
  (* Creation of a variable name of the shape x_N, and recording in vars *)
  let new_var id =
    incr cpt;
    let v = Printf.sprintf "%s_%i" id !cpt in
    vars := v :: !vars;
    v
  in
  
  (* Main translation function
     Return the pair (s, e'), and records variable names in vars as a side effect *)
  let rec tr_expr (e: Clj.expression) (env: string STbl.t):
      Imp.sequence * Imp.expression =
    match e with
      | Clj.Int(n) ->
         [], Imp.Int(n)

      | Clj.Bool(b) ->
         [], Imp.Bool(b)

      | Clj.Var(v) ->
         [], tr_var v env

      | Clj.Unop(op, e) ->
         let is, te = tr_expr e env in
         is, Imp.Unop(op, te)
         
      | Clj.Binop(op, e1, e2) ->
         let is1, te1 = tr_expr e1 env in
         let is2, te2 = tr_expr e2 env in
         is1 @ is2, Imp.Binop(op, te1, te2)

      | Clj.MkClj(fname, vars) ->
         let lv = new_var "closure" in
         let closure_size = List.length vars + 1 in
         let alloc_instr = Imp.Set(lv, Imp.array_create (Imp.Int closure_size)) in
         let set_fname = Imp.array_set (Imp.Var lv) (Imp.Int 0) (Imp.Addr fname) in
         let set_vars =
            List.mapi
               (fun i var -> Imp.array_set (Imp.Var lv) (Imp.Int (i + 1)) (tr_var var env))
               vars
         in
         Imp.(
            alloc_instr :: set_fname :: set_vars, Imp.Var lv)
   
      | Clj.App(f, arg) ->
         let is1, tf = tr_expr f env in
         let is2, targ = tr_expr arg env in
         let lv_result = new_var "call_result" in
         let call_instr =
            match tf with
            | Imp.Var fname when String.starts_with ~prefix:"fun_" fname ->
               Imp.Call(fname, [targ])
            | Imp.Var closure_name -> 
               Imp.PCall(
                  Imp.Deref(Imp.array_get (Imp.Var closure_name) (Imp.Int 0)), 
                  [targ; Imp.Var closure_name]
               )
            | _ ->
               failwith "Invalid function application: not a closure or direct function"
         in
         Imp.(is1 @ is2 @ [Set(lv_result, call_instr)], Var lv_result)
           
           

      | Clj.If(cond, e1, e2) ->
         let is1, tcond = tr_expr cond env in
         let is2, te1 = tr_expr e1 env in
         let is3, te2 = tr_expr e2 env in
         let lv = new_var "if_result" in
         Imp.(
            is1 @ [If(tcond, is2 @ [Set(lv, te1)], is3 @ [Set(lv, te2)])],
            Var lv)

      | Clj.Let(x, e1, e2) ->
         (* Creation of a unique name for 'x', to be used instead of 'x'
            in the expression e2. *)
         let lv = new_var x in
         let is1, t1 = tr_expr e1 env in
         let is2, t2 = tr_expr e2 (STbl.add x lv env) in
         Imp.(is1 @ [Set(lv, t1)] @ is2, t2)
      
      | Clj.Fix(f, body) ->
         let lv = new_var f in
         let is_body, tbody = tr_expr body (STbl.add f lv env) in
         Imp.(is_body @ [Set(lv, tbody)], Var lv)
      
      
      | _ ->
         failwith "todo"

  in
    
  let is, te = tr_expr e env in
  
  is, te, !vars

    
(* Translation of a global function *)
let tr_fdef fdef =
  let env =
    let x = Clj.(fdef.param) in
    (* The parameter 'x' is renamed 'param_x' *)
    STbl.add x ("param_" ^ x) STbl.empty
  in
  (* The variables created for the translation of the body of the function
     are the local variables of the function *)
  let is, te, locals = tr_expr Clj.(fdef.body) env in
  Imp.({
    name = Clj.(fdef.name);
    code = is @ [Return te];
    (* Two parameters: the actual argument, and the closure *)
    params = ["param_" ^ Clj.(fdef.param); "closure"];
    locals = locals;
  })


(* Translation of a full program *)
let translate_program prog =
  let functions = List.map tr_fdef Clj.(prog.functions) in
  (* Variables of the main expression are the global variables of the program *)
  let is, te, globals = tr_expr Clj.(prog.code) STbl.empty in
  (* Main code ends after printing the result of the main expression *)
  let main = Imp.(is @ [Expr(Call("print_int", [te]))]) in
  Imp.({main; functions; globals})
    
