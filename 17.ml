(* 17.1 *)
(* nenrei nengou_t nengou_t -> int*)
(* type nengou_t = Meiji of int
| Taisho of int
| Showa of int
| Heisei of int;;

let nenrei birth current = 
  let to_seireki nengou = match nengou with
  Meiji (n) -> n + 1867
  | Taisho (n) -> n + 1911
  | Showa (n) -> n + 1925
  | Heisei (n) -> n + 1985 in
  to_seireki current - to_seireki birth;;

let test = nenrei (Meiji(5)) (Meiji(10)) = 5;;
let test = nenrei (Meiji(5)) (Taisho(10)) = 1911 + 10 - 1867 - 5;; *)

(* 17.2 *)
(* type  *)
(* type year_t =
  Jan of int |
  Feb of int |
  Mar of int |
  Apr of int |
  May of int |
  Jun of int |
  Jul of int |
  Aug of int |
  Sep of int |
  Oct of int |
  Nov of int |
  Dec of int ;; *)

(* 17.3 *)

(* 17.5 *)
  type 'a tree_t = Empty
              | Leaf of 'a
              | Node of 'a * 'a tree_t * 'a tree_t ;;
(* tree_double tree_t -> tree_t *)
(* let rec tree_double t = match t with
  Empty -> Empty
| Leaf(n) -> Leaf(2 * n)
| Node(n, t1, t2) -> Node(2 * n, tree_double t1, tree_double t2);;

let test = tree_double Empty = Empty
let test = tree_double (Leaf(2)) = (Leaf(4))
let test = tree_double (Node(2, Leaf(2), Empty)) = (Node(4, Leaf(4), Empty))   *)

(* 17.6 *)
(* tree_map f tree_t -> tree_t *)
(* let rec tree_map f t = match t with
  Empty -> Empty
| Leaf(n) -> Leaf(f n)
| Node(n, t1, t2) -> Node(f n, tree_map f t1, tree_map f t2);;

let test = tree_map (fun n -> 2 * n) (Leaf(2)) = (Leaf(4)) *)

(* 17.7 *)
(* let rec tree_length t = match t with
  Empty -> 0
| Leaf(n) -> 1
| Node(n, t1, t2) -> 1 + tree_length t1 + tree_length t2;;

let test = tree_length (Node(2, Leaf(2), Empty)) = 2 *)

(* 17.8 *)
(* tree_depth tree_t -> int *)
(* let rec tree_depth t = match t with
  Empty -> 0
| Leaf(n) -> 0
| Node(n, t1, t2) -> 1 + max (tree_depth t1) (tree_depth t2);;

let test = tree_depth (Node(2, Leaf(2), Empty)) = 1 *)

(* 17.9 *)
(* sum_tree  *)
let rec sum_tree t = match t with
  Empty -> 0
| Leaf (n) -> n
| Node (n, t1, t2) -> n + sum_tree t1 + sum_tree t2;;
