module Coord = struct
  let x = 1.0
  let x = 2.0
end
(* module Coord : sig val x : float end *)

module Tree = struct
  type ('a, 'b) t = Empty
                  | Node of ('a, 'b) t * 'a * 'b * ('a, 'b) t

  let empty = Empty

  let rec insert tree k v = match tree with
      Empty -> Node (Empty, k, v, Empty)
    | Node (left, key, value, right) ->
       if k = key then Node (left, key, v, right)
       else if k < key
       then Node (insert left k v, key, value, right)
       else Node (left, key, value, insert right k v)

  let rec search tree k = match tree with
      Empty -> raise Not_found
    | Node (left, key, value, right) ->
       if k = key then value
       else if k < key then search left k
       else search right k
end

(*
module Tree : sig
  type ('a, 'b) t = Empty | Node of ('a, 'b) t * 'a * 'b * ('a, 'b) t
  val empty : ('a, 'b) t
  val insert : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t
  val search : ('a, 'b) t -> 'a -> 'b
end
*)
