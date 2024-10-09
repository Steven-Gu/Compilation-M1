(* We test the uop Minus, Add the bop | Sub | Mul | Div | Rem | Lsl | Lsr *)
let x = ((((((-1 + 4) / 2) * 5) mod 3) << 3)) >> 1 in
(* ((((3 / 2) * 5) mod 3) << 3) >> 1 = ((5 mod 3) << 3) >> 1 = (2 << 3) >> 1 = 8 *)
x