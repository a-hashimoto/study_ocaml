(* 22-1 *)
(* gensym string -> string *)
let c = ref 0
let gensym s = 
  (
  c := !c + 1;
  s ^ (string_of_int(!c - 1))
  )
let test = gensym "a"
let test = gensym "a"
let test = gensym "a"

(* 22-2 *)
let count = ref 0
let rec fib_array a = 
  if Array.length a = !count then a else
  if !count = 0 then (count := 1; fib_array a)
  else if !count = 1 then (count := 2; (a.(1) <- 1); fib_array a)
  else (count := !count + 1; a.(!count - 1) <- (a.(!count - 2) + a.(!count - 3)); fib_array a);;

let test = fib_array [|0; 0; 0; 0|]