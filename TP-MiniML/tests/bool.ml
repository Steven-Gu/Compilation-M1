(* Test | Lt | Le | Gt | Ge | And *)
let a = (0 < 1) && (0 <= 0) && (1 > 0) && (1 >= 1) in
(* Test | Eq | Neq *)
let b = (0 = 0) && (1 <> 0) in
(* Test | Or | Not *)
let c = (false || not false)in
(* return true if and only if a, b and c are all true*)
a && b && c
