let main n =
  let result = Fac.f n in
    print_int result

let _ = main (int_of_string Sys.argv.(1))
