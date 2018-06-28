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

(* 14.8 *)
(* no_name_fun int -> int *)
fun x -> x * x - 1;;

(* 14.9 *)
(* 人間一人分のデータ (身長、体重、誕生日の月と日、血液型)を表す型*)
type person_t = {
    name : string; (* 名前 *)
    height : int; (*身長*)
    weight : int; (*体重*)
    birthday_month : int; (*誕生日の月*)
    birthday_day : int; (*誕生日の日*)
    blood_type : string; (*血液型*)
};;


let akira = {
    name = "akira";
    height = 169;
    weight = 66;
    birthday_month = 3;
    birthday_day = 10;
    blood_type = "A"
};;

(* fun person_t -> string *)
fun p -> p.name;;

(* 14.10 *)

(* 14.15 *)
let rec enumerate n = if n = 0 then [] else n :: enumerate(n-1);;

(* one_to_n int -> int *)
let one_to_n n = List.fold_right (+) (enumerate n) 0;;
(* let test = one_to_n 3 = 6;; *)

(* 14.16 *)
(* fac int -> int *)
let fac n = List.fold_right ( * ) (enumerate n) 1;;
let test = fac 3 = 6;;