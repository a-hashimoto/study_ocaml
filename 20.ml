(* 20-1 *)
type color_t = Red | Black;;
(* rb_tree_t *)
type ('a, 'b) t =
    Empty
    | Node of 'a * 'b * color_t * ('a, 'b) t * ('a, 'b) t

(* 20-2 *)
(* balance rb_tree_t -> rb_tree_t *)
let balance tree = match tree with
    Node (zk, zv, Black, Node (yk, yv, Red, Node (xk, xv, Red, a, b), c), d)
    | Node (zk, zv, Black, Node (xk, xv, Red, a, Node (yk, yv, Red, b, c)), d)
    | Node (xk, xv, Black, a, Node (zk, zv, Red, Node (yk, yv, Red, b, c), d))
    | Node (xk, xv, Black, a, Node (yk, yv, Red, b, Node (zk, zv, Red, c, d)))
        -> Node (yk, yv, Red, Node (xk, xv, Black, a, b), Node (zk, zv, Black, c, d))
    | _ -> tree

(* 20-3 *)
let rec insert tree key value = 
  let rec set t0 = match t0 with
  Empty -> Node (key, value, Red, Empty, Empty)
  Node (k, v, color, a, b) -> if value = v then Node (key, v, color, a, b)
                                    else if value < v then balance(Node (k, v, color, set a, b))
                                    else balance(Node (k, v, color, a, set b)) in
  match set tree with
  Empty -> Assert_failure
  | Node (k, v, color, a, b) -> Node (k, v, Black, a, b);;

(* 20-7 *)
module type Set = sig    type 'a t

    val empty : 'a t

    val singleton : 'a -> 'a t

    val to_set : 'a list -> 'a t

    val to_list : 'a t -> 'a list

    val union : 'a t -> 'a t -> 'a t

    val inter : 'a t -> 'a t -> 'a t

    val diff : 'a t -> 'a t -> 'a t

    val mem : 'a -> 'a t -> bool
    
end = struct

end