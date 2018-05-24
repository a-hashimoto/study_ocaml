(* is_even int -> bool *)
(* even int lst -> int lst *)

let even l = 
  let is_even i = i mod 2 = 0 in
    List.filter is_even l;;
 
 (* let test = even [0] = [0]
let test = even [0 ;1] = [0]
let test = List.length(even [0;1;2;3]) = 2  *)

type gakusei_t = {
  namae : string;
  tensuu : int;
  seiseki : string;
}

(* is_seiseki_A gakusei_t -> bool *)

(* count_A gakusei_t lst -> int*)
let count_A l =
  let is_seiseki_A g = g.seiseki = "A" in
  List.length(List.filter is_seiseki_A l);;

(* let test = count_A [{
    namae = "akira";
  tensuu : 1;
  seiseki : "A"
}] = 1;; *)

(* concat0 string string-> string *)
(* concat string lst -> string *)
let concat l = 
  let concat0 s1 s2 = s1 ^ s2 in
  List.fold_right concat0 l "";;
(* let test = concat ["1" ; "2"] = "12";; *)

(* seiseki_sum gakusei_t -> int -> int *)
(* gakusei_sum gakusei lst -> int *)
let gakusei_sum l = 
  let seiseki_sum g1 i = g1.tensuu + i in
  List.fold_right seiseki_sum l 0;;
(* let test = gakusei_sum [{namae = ""; tensuu = 1; seiseki = ""};{namae = ""; tensuu = 1; seiseki = ""}] = 2;; *)



let student1 = {
  namae = "a" ;
  tensuu = 1;
  seiseki = "SSR";
}
(* count : student_list -> string -> int *)
let count l s = 
  (* eval s -> s -> bool *)
  let eval s1 = s1.seiseki = s in
    List.length (List.filter eval l);;

(* let test = count [student1; student1] "SSR" = 2;; *)

