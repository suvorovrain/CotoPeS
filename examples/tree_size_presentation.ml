(* Упражнение 1 с лекции про CPS. Замените реализации run, return и let∗, чтобы появилась оптимизация
хвостовой рекурсии для вычисления размера дерева.*)

type tree =
  | Leaf
  | Node of tree * tree

let return x = fun k -> k x
let run x k = k x
let ( let* ) x f = fun k -> x (fun v -> f v k)

let size =
  let rec helper = function
    | Leaf -> return 0
    | Node (l, r) ->
      let* sl = helper l in
      let* sr = helper r in
      return (sl + sr + 1)
      (* если всё расписать через определения выше, будет (fun k -> 
        helper l (fun sl -> 
          helper r (fun sr -> 
            k (sl + sr + 1)))) *)
      (* Родион, проверь что я тут не солгал *)
  in
  fun root -> run (fun n -> n) (helper root)
;;

let test_tree = Node (Node (Node (Leaf, Leaf), Leaf), Node (Leaf, Leaf))

let%expect_test "Simple size evaluating" =
  print_endline (string_of_int (size test_tree));
  [%expect {| 4 |}]
;;
