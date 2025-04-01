let rec revk lst k =
  match lst with
  | [] -> k []
  | hd :: tl -> revk tl (fun r -> k (r @ [ hd ]))
;;

(*
   revk (1 :: 2 :: 3 :: []) k0
= revk (2 :: 3 :: []) (fun xs −> k0 (xs @ [1]))
= revk (3 :: []) (fun xs −> k0 ((xs @ [2]) @ [1]))
= revk [] (fun xs −> k0 (((xs @ [3]) @ [2]) @ [1]))
= k0 ((([] @ [3]) @ [2]) @ [1])
= k0 ([3; 2; 1])

After simplifications...
   revk (1 :: 2 :: 3 :: []) k0
= revk (2 :: 3 :: []) (fun xs −> k0 (xs @ [1]))
= revk (3 :: []) (fun xs −> k0 (xs @ [2; 1]))
= revk [] (fun xs −> k0 (xs @ [3; 2; 1]))
= k0 ([3; 2; 1])

Let's replace (fun xs −> k0 (xs @ acc)) with (acc: 'a list)
*)

let rec rev1 list acc =
  match list with
  | [] -> apply acc []
  | hd :: tl -> rev1 tl (hd :: acc)

and apply f x = f @ x

let%expect_test "revk" =
  List.iter (fun i -> Printf.printf "%d " i) (revk [ 4; 7; 9; 0 ] Fun.id);
  [%expect {| 0 9 7 4 |}]
;;

let%expect_test "rev1" =
  List.iter (fun i -> Printf.printf "%d " i) (rev1 [ 4; 7; 9; 0 ] []);
  [%expect {| 0 9 7 4 |}]
;;
