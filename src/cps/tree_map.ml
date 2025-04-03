(* Задача: написать tailrec map для дерева
    1) один тип-параметр 'a
    2) в листьях хранятся тройки из 'a
    3) в узлах два поддерева типа ('a * 'a) t
    4) аннотации типов можно опустить *)

(* тип дерева *)
type 'a t =
  | Leaf of 'a * 'a * 'a
  | Node of ('a * 'a) t * ('a * 'a) t

(* Решение от старших курсов, подкорректировано с типизацией *)
let rec mapk : 'a 'b 'c. ('a -> 'b) -> 'a t -> ('b t -> 'c) -> 'c =
  fun f t k ->
  match t with
  | Leaf (l1, l2, l3) -> k (Leaf (f l1, f l2, f l3))
  | Node (t1, t2) ->
    let g = fun (x, y) -> f x, f y in
    mapk g t1 (fun nt1 -> mapk g t2 (fun nt2 -> k (Node (nt1, nt2))))
;;

let rec print_tree : 'a. ('a -> unit) -> 'a t -> unit =
  fun print_elt t ->
  match t with
  | Leaf (a, b, c) ->
    let () = Printf.printf "Leaf (" in
    let () = print_elt a in
    let () = Printf.printf ", " in
    let () = print_elt b in
    let () = Printf.printf ", " in
    let () = print_elt c in
    Printf.printf ")\n"
  | Node (l, r) ->
    let print_pair (x, y) =
      let () = Printf.printf "(" in
      let () = print_elt x in
      let () = Printf.printf "," in
      let () = print_elt y in
      Printf.printf ")"
    in
    let () = print_tree print_pair l in
    print_tree print_pair r
;;

let test_tree = Node (Leaf ((1, 1), (2, 2), (3, 3)), Leaf ((3, 3), (4, 4), (5, 5)))
let mapped_tree = mapk Int.succ test_tree Fun.id

let%expect_test _ =
  print_tree print_int mapped_tree;
  [%expect
    {|
      Leaf ((2,2), (3,3), (4,4))
      Leaf ((4,4), (5,5), (6,6)) |}]
;;
