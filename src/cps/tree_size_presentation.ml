(* Упражнение 1 с лекции про CPS. Замените реализации run, return и let∗, чтобы появилась оптимизация
хвостовой рекурсии для вычисления размера дерева.*)

type tree =
  | Leaf
  | Node of tree * tree

let return x = fun k -> k x
let run x k = k x
let ( let* ) x f = fun k -> x (fun v -> (f v k))

let size =
  let rec helper = function
    | Leaf -> return 0
    | Node (l, r) ->
      let* sl = helper l in
      let* sr = helper r in
      return (sl + sr + 1) 
  in
  fun root -> (run[@tailcall]) (fun n -> n) (helper root)
;;

let test_tree = Node (Node (Node (Leaf, Leaf), Leaf), Node (Leaf, Leaf))

let%expect_test "Simple size evaluating" =
  print_endline (string_of_int (size test_tree));
  [%expect {| 4 |}]
;;

let rec makek =
  (fun depth k ->
  if depth <= 0 then k Leaf
  else 
    makek (depth-1) (fun r -> (fun l -> k (Node (l,r))) (if depth = 2 then r else Leaf) ))

let%expect_test "Apply function to huge tree" =
  (try print_endline (string_of_int (size (makek 1000000 Fun.id))) with
   | Stack_overflow -> print_endline "Stack overflow!");
  [%expect {| 1000001 |}]
;;
