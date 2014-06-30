(* あれ, let recにしてもしなくても動いた... *)
(* let contain_zero lst = match lst with *)
let rec contain_zero lst = match lst with
    [] -> false
  | first :: rest -> if first = 0 then true
                     else contain_zero rest

let test1 = contain_zero [] = false
let test2 = contain_zero [0; 2] = true
let test3 = contain_zero [1; 2] = false
let test4 = contain_zero [1; 2; 0; 4; 5] = true
let test5 = contain_zero [1; 2; 3; 4; 5] = false


let rec sum lst = match lst with
    [] -> 0
  | first :: rest -> first + sum rest;;

let testsum1 = sum [] = 0
let testsum2 = sum [0; 2] = 2
let testsum3 = sum [1; 2] = 3
let testsum5 = sum [1; 2; 3;] = 6
