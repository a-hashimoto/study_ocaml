(* 16-1 *)
(* sum_list int list -> int list *)
(* sum_list整数のリストを受け取ったら、それまでリストの中で出てきたかずの合計からなるリストを返す *)
let sum_list lst = 
(* hojo int list int -> int list *)
  let rec hojo lst total0 = match lst with
  [] -> []
  | first :: rest -> (first + total0) :: hojo rest (first + total0)
  in hojo lst 0;;

(* let test = sum_list [0; 3; 2; 8] *)

(* 16-2 *)
(* fold_left f init lst -> ? *)
let rec fold_left f init lst = match lst with
[] -> init
| first :: rest -> fold_left f (f init first) rest;;

(*  *)
let test = fold_left (fun x y -> x ^ y) "k" ["a"; "b"; "c"];;
let test = List.fold_left (fun x y -> x ^ y) "k" ["a"; "b"; "c"];;
let test = List.fold_right (fun x y -> x ^ y) ["a"; "b"; "c"] "k";;