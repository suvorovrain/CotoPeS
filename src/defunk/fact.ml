let rec factk x k = if x = 1 then k 1 else factk (x - 1) (fun p -> k (x * p))

(*
   factk 4 k0
= factk 3 (fun y −> k0 (4 * y))
= factk 2 (fun y −> k0 (4 * (3 * y)))
= factk 1 (fun y −> k0 (4 * (3 * (2 * y))))
= k0 (4 * (3 * (2 * 1)))
= k0 24

After simplifications...
   factk 4 k0
= factk 3 (fun y −> k0 (4 * y))
= factk 2 (fun y −> k0 (12 * y))
= factk 1 (fun y −> k0 (24 * y))
= k0 24

Let's replace (fun y −> k0 (n * y)) with (n:int)
*)

let rec fact1 x n = if x = 1 then apply n 1 else fact1 (x - 1) (n * x)
and apply f x = f * x

let%expect_test "factk" =
  print_int (factk 10 Fun.id);
  [%expect {| 3628800 |}]
;;

let%expect_test "fact1" =
  print_int (fact1 10 1);
  [%expect {| 3628800 |}]
;;
