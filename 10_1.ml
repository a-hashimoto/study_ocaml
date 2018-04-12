(* insert list int -> int -> list int *)
let rec insert lst n = match lst with
[] -> n :: []
| first :: rest -> if first < n then first :: insert rest n
                                    else n :: first :: rest ;;
(* 10-1 *)
(* let test1 = insert [] 1 = [1]
let test2 = insert [1; 2] 0 = [0; 1; 2]
let test3 = insert [0; 2] 1 = [0; 1; 2]
let test4 = insert [0; 1] 2 = [0; 1; 2] *)

(* ins_sort list int -> list int *)

let rec ins_sort lst = match lst with
[] -> []
| first :: rest -> insert (ins_sort rest) first ;;

(* 10-2 *)
let test1 = ins_sort [] = []
let test2 = ins_sort [0; 1] = [0; 1]
let test3 = ins_sort [1; 0] = [0; 1]
let test4 = ins_sort [1; 3; 2] = [1; 2; 3]
let test5 = ins_sort [3; 2; 1] = [1; 2; 3]