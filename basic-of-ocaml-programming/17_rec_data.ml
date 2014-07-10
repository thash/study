type jp_t = Meiji of int
          | Taisho of int
          | Showa of int
          | Heisei of int

(* val to_ad : jp_t -> int = <fun> *)
let to_ad jp_t = match jp_t with
    Meiji  (n) -> n + 1887
  | Taisho (n) -> n + 1911
  | Showa  (n) -> n + 1925
  | Heisei (n) -> n + 1988

to_ad(Meiji(4));;
(* - : int = 1891 *)

type tree_t = Empty
            | Leaf of int
            | Node of tree_t * int * tree_t (* cons cell *)

(* Node (Node (Empty, 7, Leaf (3)), 17, Leaf (24));;
   - : tree_t = Node (Node (Empty, 7, Leaf 3), 17, Leaf 24) *)

let rec sum_tree tree = match tree with
    Empty -> 0
  | Leaf (n) -> n
  | Node (left, m, right) -> m + (sum_tree right) + (sum_tree right)

(* sum_tree (Node (Node (Empty, 7, Leaf (3)), 17, Leaf (24)));;
   - : int = 65 *)

(* まず格納されている構造に関係ない検索 *)
(* val _search : tree_t -> int -> bool = <fun> *)
let rec _search tree data = match tree with
    Empty -> false
  | Leaf (m) ->
     if data = m then true else false
  | Node (left, m, right) ->
     if data = m
     then true
     else (_search left data) || (_search right data)

let t = (Node (Node (Empty, 2, Leaf (3)), 17, Leaf (24)));;
_search t 2;;
_search t 10;;


let rec search tree data = match tree with
    Empty -> false
  | Leaf (m) ->
     if data = m then true else false
  | Node (left, m, right) ->
     if data = m
     then true
     else if data < m
          then search left data
          else search right data

search t 2;;
search t 10;;


(* val insert_tree : tree_t -> int -> tree_t = <fun> *)
let rec insert_tree tree data = match tree with
    Empty -> Leaf (data)
  | Leaf (n) ->
     if data = n
     then Leaf (n)
     else if data < n
          then Node (Leaf (data), n, Empty)
          else Node (Empty, n, Leaf (data))
  | Node (left, n, right) ->
     if data = n
     then Node (left, n, right) (* 変化なし *)
     else if data < n
          then Node (insert_tree left data, n, right)
          else Node (left, n, insert_tree right data)

insert_tree t 10;;
(* - : tree_t = Node (Node (Empty, 2, Node (Empty, 3, Leaf 10)), 17, Leaf 24) *)

type 'a pol_tree_t = Empty
                   | Leaf of 'a
                   | Node of 'a pol_tree_t * 'a * 'a pol_tree_t

Node (Empty, "hoge", Leaf ("fuga"));;
(* - : string pol_tree_t = Node (Empty, "hoge", Leaf "fuga") *)
