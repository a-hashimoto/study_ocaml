let rec sum_of_square n =
  if n = 0 then 0
  else n * n + sum_of_square (n - 1) ;;

  let test1 = sum_of_square 0 = 0
  let test2 = sum_of_square 1 = 1
  let test1 = sum_of_square 2 = 5