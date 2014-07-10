let reverse lst =
  let rec rev lst result = match lst with
      [] -> result
    | x::xs -> rev xs (x::result)
  in rev lst [] (* 内部手続revの初期実行 *)

(* reverse [1; 2; 3; 4];;
   - : int list = [4; 3; 2; 1] *)

