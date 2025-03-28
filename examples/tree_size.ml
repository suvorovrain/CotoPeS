(* tree size *)
type tree =
  | Leaf
  | Node of tree * tree

let test_tree = Node (Node (Node (Leaf, Leaf), Leaf), Node (Leaf, Leaf))

let size root =
  let rec helper = function
    | Leaf -> 0
    | Node (l, r) -> 1 + helper l + helper r
  in
  helper root
;;

let rec sizek root k =
  match root with
  | Leaf -> k 0
  | Node (l, r) -> sizek l (fun s1 -> sizek r (fun s2 -> k (s1 + s2 + 1)))
;;

let%expect_test _ =
  print_endline (string_of_int (size test_tree));
  [%expect {| 4 |}]
;;

let%expect_test _ =
  print_endline (string_of_int (sizek test_tree (fun x -> x)));
  [%expect {| 4 |}]
;;
