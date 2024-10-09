open Miniml

module Env = Map.Make(String)
(* typing environment for variables *)
type tenv = typ Env.t
(* typing environment for constructors *)
type senv = (typ list * string) Env.t
(*Env.find <constructor name> senv ===> (<argument types>, <name of constructed type>)*)
(*Env.find "N"                senv ===> ([TStruct "tree"; TStruct "tree"], "tree")*)

let typ_expr (e: expr) (senv: senv) =
  let rec typ (e: expr) (tenv: tenv) = match e with
    | Int _ -> TInt
    | Bool _ -> TBool
    | Uop(op, e) ->
      begin match op, typ e tenv with
        | Not, TBool -> TBool
        | Minus, TInt -> TInt
        | Fst , TPair(t1, _) -> t1
        | Snd , TPair(_, t2) -> t2
        | _ -> failwith "type error: unop"
      end
    | Bop(op, e1, e2) ->
      begin match op, typ e1 tenv, typ e2 tenv with
        | (Add | Sub | Mul | Div | Rem), TInt, TInt -> TInt
        | (Lsl | Lsr), TInt, TInt -> TInt
        | (Lt | Le | Gt | Ge), TInt, TInt -> TBool
        | (Eq | Neq), t1, t2 when t1 = t2 -> TBool
        | (And | Or), TBool, TBool -> TBool
        | Pair, t1, t2 -> TPair(t1, t2)
        | _ -> failwith "type error: binop"
      end
    | Var(x) -> Env.find x tenv
    | Let(x, e1, e2) ->
      let t1 = typ e1 tenv in
      typ e2 (Env.add x t1 tenv)
    | If(c, e1, e2) ->
      begin match typ c tenv, typ e1 tenv, typ e2 tenv with
        | TBool, t1, t2 when t1 = t2 -> t1
        | _ -> failwith "type error: if"
      end
    | App(e1, e2) ->
      begin match typ e1 tenv, typ e2 tenv with
        | TFun(ta, t1), t2 when ta = t2 -> t1
        | _ -> failwith "type error: application"
      end
    | Fun(x, t, e) ->
      TFun(t, typ e (Env.add x t tenv))
    | Fix(x, t, e) ->
      if typ e (Env.add x t tenv) = t then
        t
      else
        failwith "type error: fix"
    | Cstr(c, args) ->
      (* c is assumed to be in senv *)
      (* args have to be checked against arg types of c *)
      (* final type : what is constructed *)
      (*Env.find <constructor name> senv ===> (<argument types>, <name of constructed type>)*)
      let (ls,s) = try Env.find c senv with Not_found -> failwith (c ^ " not found in senv") in
      let rec args_typ_check args ls =
        match args, ls with 
          | [], [] -> ()
          | [], _  -> failwith ("missing arguments in Cstr " ^ c)
          | _,  [] -> failwith ("too much arguments in Cstr " ^ c)
          | e::es, t::ts -> 
            if (typ e tenv) = t then 
              args_typ_check es ts 
            else 
              failwith "incorrect type of arguments"
      in
      args_typ_check args ls; TStruct s 
      
    
    | Match(e, cases) ->
      (* e has some type t *)
      (* all patterns in cases have to be compatible with t *)
      (* all cases must produce results of the same type (not necessarily t) *)

      (* simplifying assumptions: a pattern is either
          - C
          - C(x1, ... , xN)
          (no nesting)
      *)
      let t = typ e tenv in
      let rec typ_case (pat, e') tenv =
        match pat with
          | PVar v -> typ e' (Env.add v t tenv)
          | PCstr(cstr, pls) ->
              let (ls,s) = try Env.find cstr senv with Not_found 
                -> failwith (cstr ^ " not found in senv") in
              if TStruct s <> t then failwith ("pattern " ^ cstr ^ " not compatible with t")
              else if List.length pls <> List.length ls then
                failwith ("pattern arity mismatch for constructor " ^ cstr)
              else
                let rec check_patterns pat_list typ_list tenv =
                  match pat_list, typ_list with
                  | [], [] -> tenv
                  | PVar v :: ps, ty :: tys -> 
                      check_patterns ps tys (Env.add v ty tenv)
                  | PCstr _ :: _, _ -> failwith "nested constructor patterns are not supported"
                  | _, _ -> failwith "unexpected pattern length mismatch"
                in
                let extended_tenv = check_patterns pls ls tenv in
                typ e' extended_tenv
      in
      let case_types = List.map (fun (pat, e') -> typ_case (pat, e') tenv) cases in
      let rec check_same_type = function
        | [] | [_] -> ()
        | t1 :: t2 :: ts ->
            if t1 = t2 then check_same_type (t2 :: ts)
            else failwith "cases do not return the same type "
      in
      check_same_type case_types;
      List.hd case_types

  in
  typ e Env.empty
  
let typ_prog p =
  let rec add_types typs env =
      match typs with
      | [] -> env
      | (type_name, cstr_list)::ts  -> add_types ts (add_cstr type_name cstr_list env)
  and add_cstr type_name cstr_list env =
      List.fold_left (fun env (cstr_name, arg_types) -> 
        Env.add cstr_name (arg_types,type_name) env) env cstr_list
  in
  let senv = add_types p.typs Env.empty in
  typ_expr p.code senv
