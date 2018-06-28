(* 15.1 *)
let rec quick_sort lst =
  let take n lst p = List.filter (fun item -> p item  n)lst 
  in let take_less n lst = take n lst (<)
  in let take_greater n lst = take n lst (>)
  in let take_equal n lst = take n lst (=)
  in match lst with 
    [] -> []
    | first :: rest -> quick_sort (take_less first rest)
                      @ [first]
                      @ quick_sort (take_equal first rest)
                      @ quick_sort (take_greater first rest);;
                      
let test = quick_sort [0; 0; 1] = [0; 0; 1];;

(* 15.2 *)
(* 自明 if m = 0 その場合の答え m 最大公約数 *)
(* 部分問題　 *)
let rec gcd a b = 
  let c = a mod b in
  if c = 0 then b
  else gcd b c;;

let test1 = gcd 8 3 = 1;;
let test1 = gcd 8 4 = 4;;

(* 15.3 *)
(* prime int lst -> int lst *)
let rec sieve l = match l with
  [] -> []
  | first :: rest -> let divisible i = i mod first <> 0 in
  first :: sieve (List.filter divisible rest);;
let test = sieve [2; 3; 4] = [2; 3]
let test = sieve [2; 4] = [2]

let rec make_under_n_list k = if k = 1 then []
else k :: make_under_n_list (k - 1);;


let rec insert lst n = match lst with
[] -> n :: []
| first :: rest -> if first < n then first :: insert rest n
                                    else n :: first :: rest ;;

let rec ins_sort lst = match lst with
[] -> []
| first :: rest -> insert (ins_sort rest) first ;;

let prime n = sieve (ins_sort(make_under_n_list n));;
 let test = prime 5