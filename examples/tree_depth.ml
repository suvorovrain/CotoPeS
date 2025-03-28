(* tree depth *)
type tree =
| Leaf
| Node of tree * tree

let test_tree = Node (Node ( Node (Leaf, Leaf), Leaf), Node (Leaf,Leaf))

let rec depth root =
  match root with
  | Leaf -> 0
  | Node (l,r) -> 1 + max (depth l) (depth r) 

let maxk x y k = if x >= y then k x else k y

let rec depthk root k =
    match root with
    | Leaf -> k 0
    | Node (l,r) -> depthk l (fun s1 -> depthk r (fun s2 -> maxk s1 s2 (fun s3 -> k (s3 + 1)))) 

let%expect_test _ = print_int (depth test_tree);
[%expect{| 3 |}]

let%expect_test _ = print_int (depthk test_tree (fun x -> x));
[%expect{| 3 |}]