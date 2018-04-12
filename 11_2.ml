let rec a n =
  if n = 0 then 3
            else 2 * (a (n - 1)) - 1;;

let test0 = a 0 = 3
let test1 = a 1 = 5
let test2 = a 2 = 9
