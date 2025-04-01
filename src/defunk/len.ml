let rec lenk l k =
  match l with
  | [] -> k 0
  | _ :: tl -> lenk tl (fun r -> k (1 + r))
;;

(*
   lenk (1 :: 2 :: 3 :: []) k0
= lenk (2 :: 3 :: []) (fun y −> k0 (1 + y))
= lenk (3 :: []) (fun y −> k0 (1 + (1 + y)))
= lenk [] (fun y −> k0 (1 + (1 + (1 + y))))
= k0 (1 + (1 + (1 + 0)))
= k0 3

After simplifications...
lenk (1 :: 2 :: 3 :: []) k0
= lenk (2 :: 3 :: []) (fun y −> k0 (1 + y))
= lenk (3 :: []) (fun y −> k0 (2 + y))
= lenk [] (fun y −> k0 (3 + y))
= k0 3

Let's replace (fun y −> k0 (n + y)) with (n:int)
*)

let rec len1 l n =
  match l with
  | [] -> apply n 0
  | _ :: tl -> len1 tl (n + 1)

and apply f x = f + x

let%expect_test "lenk" =
  print_int (lenk [ 1; 4; 8; 7; 4; 8 ] Fun.id);
  [%expect {| 6 |}]
;;

let%expect_test "len1" =
  print_int (len1 [ 1; 4; 8; 7; 4; 8 ] 0);
  [%expect {| 6 |}]
;;
