(*even int list -> int list  *)
let rec even lst = match lst with
[] -> []
| first :: rest -> if first mod 2 = 0 then first :: even rest
                                      else even rest;;

let test1 = even[0] = [0]
let test2 = even[1; 2] = [2]
let test3 = even[1] = []