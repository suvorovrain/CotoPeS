(* tree size - количество узлов в бинарном дереве *)
type tree =
  | Leaf
  | Node of tree * tree

(* обычный поиск размера дерева не в хвостовом стиле *)
let size root =
  let rec helper = function
    | Leaf -> 0
    | Node (l, r) -> 1 + helper l + helper r
  in
  helper root
;;

(* поиск размера дерева в CPS-стиле *)
let rec sizek root k =
  match root with
  | Leaf -> k 0
  | Node (l, r) -> sizek l (fun s1 -> sizek r (fun s2 -> k (s1 + s2 + 1)))
;;

let test_tree = Node (Node (Node (Leaf, Leaf), Leaf), Node (Leaf, Leaf))

let%expect_test "Simple size evaluating" =
  print_endline (string_of_int (size test_tree));
  [%expect {| 4 |}]
;;

let%expect_test "CPS size evaluating" =
  print_endline (string_of_int (sizek test_tree Fun.id));
  [%expect {| 4 |}]
;;
