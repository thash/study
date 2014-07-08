(* 無名関数 *)
(fun x y -> x + y) 1 3;;

let rec myMap func lst = match lst with
    [] -> []
  | first :: rest -> func first :: myMap func rest;;

let print_bool var =
  if var then print_string "true" else print_string "false";;

print_bool ((myMap (fun x -> x * x) [1; 2; 3]) = [1; 4; 9]) ;;
