let rec fact x = if x = 1 then 1 else x * fact (x-1)


(* let%expect_test _ = print_int (fact 10); *)
(* let%expect_test _ = print_char 'c'; *)