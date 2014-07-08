(* tuareg-mode: C-c C-r eval region *)

let lt n lst = match lst with
    [] -> []
  | x::xs -> if x < n
             then x :: lt n xs
             else lt n xs

let gt n lst = match lst with
    [] -> []
  | x::xs -> if x > n
             then x :: gt n xs
             else gt n xs

let rec filter judge lst = match lst with
    [] -> []
  | first :: rest ->
     if judge first then first :: filter judge rest
     else filter judge rest

let take n lst p = filter (fun item -> p item n) lst

(* quick_sort : int list -> int list *)
let rec quick_sort lst = match lst with
    [] -> []
  | x::xs -> quick_sort (take x xs (<))
             @ [x]
             @ quick_sort (take x xs (>))

(* @ はリストの結合 *)

let test1 = quick_sort [] = []
let test2 = quick_sort [1] = [1]
let test3 = quick_sort [1; 2] = [1; 2]
let test4 = quick_sort [2; 1] = [1; 2]
let test5 = quick_sort [5; 4; 9; 8; 2; 3] = [2; 3; 4; 5; 8; 9]
