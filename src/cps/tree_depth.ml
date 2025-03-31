(* tree depth - вычисление глубины бинарного дерева *)
type tree =
  | Leaf
  | Node of tree * tree

(* обычная реализация не в хвостовом стиле *)

let rec depth root =
  match root with
  | Leaf -> 0
  | Node (l, r) -> 1 + max (depth l) (depth r)
;;

(* поиск наибольшего из двух чисел в CPS-стиле *)

(* CPS *)
let maxk x y k = if x >= y then k x else k y

(* поиск глубины в CPS-стиле *)

let rec depthk root k =
  match root with
  | Leaf -> k 0
  | Node (l, r) ->
    depthk l (fun s1 -> depthk r (fun s2 -> maxk s1 s2 (fun s3 -> k (s3 + 1))))
;;

let test_tree = Node (Node (Node (Leaf, Leaf), Leaf), Node (Leaf, Leaf))

let%expect_test "Simple depth evaluating" =
  print_int (depth test_tree);
  [%expect {| 3 |}]
;;

let%expect_test "CPS depth evaluating" =
  print_int (depthk test_tree Fun.id);
  [%expect {| 3 |}]
;;

let huge_tree depth =
  let rec helperk depth k =
    if depth = 0 then k Leaf else helperk (depth - 1) (fun tree -> k (Node (tree, Leaf)))
  in
  helperk depth Fun.id
;;

let%expect_test "Simple depth on huge tree" =
  (try print_endline (string_of_int (depth (huge_tree 1000000))) with
   | Stack_overflow -> print_endline "Stack overflow!");
  [%expect {| Stack overflow! |}]
;;

let%expect_test "Huge tree" =
  (try print_endline (string_of_int (depthk (huge_tree 1000000) Fun.id)) with
   | Stack_overflow -> print_endline "Stack overflow!");
  [%expect {| 1000000 |}]
;;
