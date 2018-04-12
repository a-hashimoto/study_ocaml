(* concat string list -> string  *)
let rec concat lst = match lst with
[] -> ""
| first :: rest -> first ^ concat rest;;

let test1 = concat[] = "";;
let test2 = concat["a"] = "a";;
let test3 = concat["a"; "b"] = "ab";;