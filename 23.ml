module type Heap : sig
  type ('a, 'b) t
  (* 最小値を求める値が'a型で *)
  (* そのほかの付加情報が'b型であるヒープの型 *)
  
  val create : int -> 'a -> 'b -> ('a, 'b) t
  (* 使い方: create size key value *)
  (* ヒープのサイズと'aと'b型のダミーの値を受け取ったらからのヒープを返す *)

  val insert : ('a, 'b) t -> 'a -> 'b -> index_t * ('a, 'b) t
  (* 使い方: insert heap key value *)
  (* ヒープに新しい要素を追加する *)
  (* ヒープは（破壊的に）書き換わる *)

  type index_t
  (* ヒープの添え字の型 *)

  val get : ('a, 'b) t -> index_t -> 'a * 'b
  (* 使い方：get heap index *)
  (* ヒープのindex番目の要素を返す *)
  
  val set : ('a, 'b) t -> index_t -> 'a -> 'b -> ('a, 'b) t
  (* 使い方：set heap index key value *)
  (* ヒープのindex番目の値を更新したヒープを返す *)
  (* ヒープは（破壊的に）書き換わる *)

  val split_top : ('a, 'b) t -> ('a * 'b) * ('a, 'b) t
  (* 使い方：split_top heap *)
  (* 師匠の値を持つものとそれを取り除いたヒープの組を返す *)
  (* ヒープは（破壊的に）書き換わる *)

end = struct
  (* ヒープの添え字の型 *)
　type index_t = int ref

  (* 最小値を求める値が'a型でそのほかの付加情報が'b型であるヒープの型 *)
  type ('a, 'b) t = int ref * (index_t * 'a * 'b) array

  val create i k v = (k, v) t

end