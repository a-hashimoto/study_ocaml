
(* 人間一人分のデータ (身長、体重、誕生日の月と日、血液型)を表す型*)
type person_t = {
    name : string; (* 名前 *)
    height : int; (*身長*)
    weight : int; (*体重*)
    birthday_month : int; (*誕生日の月*)
    birthday_day : int; (*誕生日の日*)
    blood_type : string; (*血液型*)
};;

(* 目的：人間のデータpersonを受け取って血液型に関するコメントを受けとる *)
(* blood_type_comment : person -> string *)

(* let blood_type_comment person = match person with {
  name = n; blood_type = b
} -> n ^ "さんの血液型は" ^ b ^ "型です" ;;

let test1 = blood_type_comment {
    name = "akira";
    height = 169;
    weight = 66;
    birthday_month = 3;
    birthday_day = 10;
    blood_type = "A"
} = "akiraさんの血液型はA型です" *)

(* 
let akira = {
    height = 169;
    weight = 66;
    birthday_month = 3;
    birthday_day = 10;
    blood_type = "A"
} *)

(* count_ketsueki_A : person_t list -> int *)
(* 
let rec count_ketueki_A lst = match lst with
    [] -> 0
    | {name = n; height = h; weight = w; birthday_month = bm; birthday_day = bd; blood_type = bt} :: rest
        -> if bt = "A" then 1 + count_ketueki_A rest
                        else count_ketueki_A rest;; *)

let rec person_sort lst = match lst with
    [] -> []
    | first :: rest
        -> if first.name > person_sort lst

let lst1 = []
let lst2 = [{
    name = "akira";
    height = 169;
    weight = 66;
    birthday_month = 3;
    birthday_day = 10;
    blood_type = "A"
} ]
let lst3 = [{
    name = "akira";
    height = 169;
    weight = 66;
    birthday_month = 3;
    birthday_day = 10;
    blood_type = "A"
} ;
{
    name = "akira";
    height = 169;
    weight = 66;
    birthday_month = 3;
    birthday_day = 10;
    blood_type = "B"
} ]
let lst4 = [{
    name = "akira";
    height = 169;
    weight = 66;
    birthday_month = 3;
    birthday_day = 10;
    blood_type = "A"
} ;
{
    name = "akira";
    height = 169;
    weight = 66;
    birthday_month = 3;
    birthday_day = 10;
    blood_type = "A"
} ;
{
    name = "akira";
    height = 169;
    weight = 66;
    birthday_month = 3;
    birthday_day = 10;
    blood_type = "B"
} ]

let test1 = count_ketueki_A lst1 = 0
let test2 = count_ketueki_A lst2 = 1
let test3 = count_ketueki_A lst3 = 1
let test4 = count_ketueki_A lst4 = 2
