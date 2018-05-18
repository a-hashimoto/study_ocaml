(*  *)
let twice f = let g a = f (f a) in g;;
let func a = a + 1;;
(*  *)
let test = twice twice ;;
let test3 x = (twice twice) x;;
let test1 = test func ;;
let foo = test;;