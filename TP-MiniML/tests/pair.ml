(* Test Pair *)
let x = (5,false) in
(* Test Fst, Snd *)
let f (x:(int * bool)) = 
  if (snd x = true) then fst x + 1 
  else fst x - 1 
in
(* Should be 4 *)
f x 