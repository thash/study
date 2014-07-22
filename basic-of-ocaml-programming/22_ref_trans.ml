(* Referential transparency *)
let count = ref 0;;
let rec fib n =
  (count := !count + 1;
   if n < 2 then n else fib (n - 1) + fib (n - 2))

(* array *)
[ 1; 2; 3 ];;
- : int list = [1; 2; 3]
[| 1; 2; 3 |];;
- : int array = [|1; 2; 3|]
