let rec add_to_each item lst = match lst with
    [] -> []
  | first :: rest -> (item :: first) :: add_to_each item rest

let test1 = add_to_each 1 [] = []
let test2 = add_to_each 1 [[2]] = [[1; 2]]
let test3 = add_to_each 1 [[2]; [2; 3]] = [[1; 2]; [1; 2; 3]]

let rec min lst = match lst with
    [] -> max_int (* int = 4611686018427387903 を表す定数 *) (* いいのか... *) (* いいのか, と思うなら18章の例外処理を使いなさいとのこと *)
  | first :: rest -> if first <= (min rest) then first
                     else min rest

let testmin1 = min [] = max_int
let testmin2 = min [1; 2; 3] = 1
let testmin3 = min [3; 2; 1] = 1
let testmin4 = min [3; 1; 2] = 1

(* let ... in, で局所変数 *)

let rec min lst = match lst with
    [] -> max_int
  | first :: rest ->
     let min_rest = min rest in
     if first <= min_rest then first
     else min_rest
