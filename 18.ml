(* 18-1 *)
type person_t = {
    name : string; (* 名前 *)
    height : int; (*身長*)
    weight : int; (*体重*)
    birthday_month : int; (*誕生日の月*)
    birthday_day : int; (*誕生日の日*)
    blood_type : string; (*血液型*)
};;
(* first_A person_t list -> option *)
let rec first_A pl = match pl with
[] -> None
| first :: rest -> if first.blood_type = "A" then Some(first)
else first_A rest;;

let akira = {
  name = "akira";
    height = 169;
    weight = 66;
    birthday_month = 3;
    birthday_day = 10;
    blood_type = "A"
}

let bakira = {
  name = "bakira";
    height = 169;
    weight = 66;
    birthday_month = 3;
    birthday_day = 10;
    blood_type = "B"
}

let okira = {
  name = "okira";
    height = 169;
    weight = 66;
    birthday_month = 3;
    birthday_day = 10;
    blood_type = "O"
}

let test = first_A [akira ; bakira] = Some(akira);;
let test = first_A [bakira] = None;;
let test = first_A [bakira ;akira ; bakira] = Some(akira);;
let test = first_A [okira ; bakira] = None;;

let yaoya_list = [("トマト", 300); ("玉ねぎ", 200); ("人参", 150); ("ほうれん草", 200); ]
let y_list1 = [("トマ", 300); ("ねぎ", 200); ("人参", 150); ("ほうん草", 200); ]
let y_list2 = [("トト", 300); ("玉ねぎ", 200); ("人参", 150); ("ほうれん草", 200); ]


let rec same ys yol = match yol with
[] -> None
| (yasai, nedan) :: rest -> if ys = yasai then Some (ys)
else same ys rest;;
(*18-2  *)
(* count_urikire_yasai yasai list yaoya list -> int *)
let rec count_urikire_yasai yasail yaoyal = match yasail with
[] -> 0
| (yasai, nedan) :: rest -> match same yasai yaoyal with
None -> 1 + count_urikire_yasai rest yaoyal
| Some(yasai) -> count_urikire_yasai rest yaoyal;;

let test = count_urikire_yasai y_list1 yaoya_list = 3;;
let test = count_urikire_yasai y_list2 yaoya_list = 1;;

(*18-3  *)
let rec ori_assoc ekimei0 lst = match lst with 
[] -> max_float
| (ekimei, kyori) :: rest ->
if ekimei = ekimei0 then kyori else assoc ekimei0 rest
let rec assoc ekimei0 lst = match lst with 
[] -> raise Not_found
| (ekimei, kyori) :: rest ->
if ekimei = ekimei0 then kyori else assoc ekimei0 rest

let test = try assoc "池袋" [] with Not_found -> true;;