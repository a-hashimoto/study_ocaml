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
  | Node (k, v, color, a, b) -> if value = v then Node (key, v, color, a, b)
                                    else if value < v then balance(Node (k, v, color, set a, b))
                                    else balance(Node (k, v, color, a, set b)) in
  match set tree with
  Empty -> Assert_failure
  | Node (k, v, color, a, b) -> Node (k, v, Black, a, b);;

(* 20-7 *)
module type MySet : sig 
    type 'a t

    val empty : 'a t

    val singleton : 'a -> 'a t

    val to_set : 'a list -> 'a t

    val to_list : 'a t -> 'a list

    val union : 'a t -> 'a t -> 'a t

    val inter : 'a t -> 'a t -> 'a t

    val diff : 'a t -> 'a t -> 'a t

    val mem : 'a -> 'a t -> bool
    
end 

module ListSet : MySet = struct
    type 'a t = Set of 'a list
    let empty = Set([])

    let singleton e = Set([e])

    let to_set l = Set(List.fold_right (fun x a -> List.mem x a then a else x::a) l [])

    let to_list s = match s with Set(l) -> l

    let union s1 s2 = match (s1, s2) with (Set(l1), Set(l2)) -> Set((List.filter (fun x -> List.mem x l2) l1) @ l2)

    let inter s1 s2 = match (s1, s2) with (Set(l1), Set(l2)) -> Set(List.filter (fun x -> List.mem x l2) l1)
    
    let diff s1 s2 = match (s1, s2) with (Set(l1), Set(l2)) -> Set(List.filter (fun x -> List.mem x l2))

    let mem e s = match s with Set(l) -> List.mem e l

end

module TreeSet : MySet = struct

type color_t = Red | Black;;
(* rb_tree_t *)
type 'a t =
    Empty
    | Node of 'a * color_t * 'a t * 'a t


let balance tree = match tree with
    Node (zv, Black, Node (yv, Red, Node (xv, Red, a, b), c), d)
    | Node (zv, Black, Node (xv, Red, a, Node (yv, Red, b, c)), d)
    | Node (xv, Black, a, Node (zv, Red, Node (yk, yv, Red, b, c), d))
    | Node (xv, Black, a, Node (yv, Red, b, Node (zv, Red, c, d)))
        -> Node (yv, Red, Node (xv, Black, a, b), Node (zv, Black, c, d))
    | _ -> tree


  let rec insert tree value = 
  let rec set t0 = match t0 with
  Empty -> Node (value, Red, Empty, Empty)
  Node (v, color, a, b) -> if value = v then Node (v, color, a, b)
                                    else if value < v then balance(Node (v, color, set a, b))
                                    else balance(Node (v, color, a, set b)) in
  match set tree with
  Empty -> Assert_failure
  | Node (v, color, a, b) -> Node (v, Black, a, b);;

  let rec search value tree = match tree with
  Empty -> false
  | Node (v, c, l, r) ->
    if value = v then true
    else if value < v then search value l
    else search value r;;

    let empty = Empty

    let singleton e = insert Empty e

    let to_set l = List.fold_right insert l Empty

    let rec to_list t = match t with
        Empty -> []
        | (v, c, l, r) -> to_list l @ v :: to_list r ;;

    let union s1 s2 = 
        let rec to_list t = match t with
        Empty -> []
        | (v, c, l, r) -> to_list l @ v :: to_list r in
        List.fold_right insert (to_list s1) s2;;
    
    let inter s1 s2 = List.fold_right (fun x t -> if search x s2 then insert x t else t ) (to_list s1) Empty;;

end