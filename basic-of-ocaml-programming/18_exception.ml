exception Zero

(* リストの数を掛け合わせて行き, 0が見つかったら例外Zeroを発生させて脱出 *)
(* val times : int list -> int = <fun> *)
let times lst =
  let rec mul lst = match lst with
      [] -> 1
    | x::xs ->
       if x = 0 then raise Zero
       else x * mul xs
  in try
    mul lst
  with Zero -> 0
