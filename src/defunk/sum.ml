let rec sumk xs k =
  match xs with
  | [] -> k 0
  | h :: tl -> sumk tl (fun s -> k (h + s))
;;

(*
   sumk (1 :: 2 :: 3 :: []) k0
= sumk (2 :: 3 :: []) (fun y −> k0 (1 + y))
= sumk (3 :: []) (fun y −> k0 (1 + (2 + y)))
= sumk [] (fun y −> k0 (1 + (2 + (3 + y))))
= k0 (1 + (2 + (3 + 0)))
= k0 6

After simplifications...
sumk (1 :: 2 :: 3 :: []) k0
= sumk (2 :: 3 :: []) (fun y −> k0 (1 + y))
= sumk (3 :: []) (fun y −> k0 (3 + y))
= sumk [] (fun y −> k0 (6 + y))
= k0 6

Let's replace (fun y −> k0 (n + y)) with (n:int)
*)

let rec sum1 xs n =
  match xs with
  | [] -> apply n 0
  | h :: tl -> sum1 tl (n + h)

and apply f x = f + x

let%expect_test "sumk" =
  print_int (sumk [ 5; 8; 7 ] Fun.id);
  [%expect {| 20 |}]
;;

let%expect_test "sum1" =
  print_int (sum1 [ 5; 8; 7 ] 0);
  [%expect {| 20 |}]
;;
