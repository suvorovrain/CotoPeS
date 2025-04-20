(* задание с офиц пересдачи 16.04.2025 *)
(* Для бинарных деревьев, где значения находятся только в листьях, реализуйте функции
   • map: 'a tree -> ('a -> 'b) -> 'b tree
   • concat: 'a tree tree -> 'a tree
   • bind — concat после map.
   Затем перепишите их в CPS (можно сразу написать в CPS стиле). Скорее всего, ничего из стандартной библиотеки не понадобится,
   но если вдруг, то стандартные рекурсивные функции тоже должны быть в CPS стиле. *)

(* сразу CPS реализация *)

type 'a tree =
  | Leaf of 'a
  | Node of 'a tree * 'a tree

let rec mapk t f k =
  match t with
  | Leaf x -> k (Leaf (f x))
  | Node (t1, t2) -> mapk t1 f (fun s1 -> mapk t2 f (fun s2 -> k (Node (s1, s2))))
;;

let rec concatk t k =
  match t with
  | Leaf x -> k x
  | Node (t1, t2) -> concatk t1 (fun s1 -> concatk t2 (fun s2 -> k (Node (s1, s2))))
;;

let bindk m f k = mapk m f (fun s -> concatk s k)

(* вспомогательные функции *)

let rec build_tree n =
  if n = 0 then Leaf 1 else Node (build_tree (n - 1), build_tree (n - 1))
;;

let print_int_tree tree =
  let rec aux prefix is_left = function
    | Leaf x ->
      Printf.printf
        "%s%sLeaf %s\n"
        prefix
        (if is_left then "├── " else "└── ")
        (string_of_int x)
    | Node (l, r) ->
      Printf.printf "%s%sNode\n" prefix (if is_left then "├── " else "└── ");
      let new_prefix = prefix ^ if is_left then "│   " else "    " in
      aux new_prefix true l;
      aux new_prefix false r
  in
  aux "" false tree
;;

let print_char_tree tree =
  let rec aux prefix is_left = function
    | Leaf x ->
      Printf.printf "%s%sLeaf %s\n" prefix (if is_left then "├── " else "└── ") x
    | Node (l, r) ->
      Printf.printf "%s%sNode\n" prefix (if is_left then "├── " else "└── ");
      let new_prefix = prefix ^ if is_left then "│   " else "    " in
      aux new_prefix true l;
      aux new_prefix false r
  in
  aux "" false tree
;;

(* тесты *)

let%expect_test "double int" =
  print_int_tree (bindk (build_tree 3) (fun x -> Leaf (x * 2)) Fun.id);
  [%expect
    {|
    └── Node
        ├── Node
        │   ├── Node
        │   │   ├── Leaf 2
        │   │   └── Leaf 2
        │   └── Node
        │       ├── Leaf 2
        │       └── Leaf 2
        └── Node
            ├── Node
            │   ├── Leaf 2
            │   └── Leaf 2
            └── Node
                ├── Leaf 2
                └── Leaf 2 |}]
;;

let%expect_test "string instead of int" =
  print_char_tree (bindk (build_tree 3) (fun _ -> Leaf "a") Fun.id);
  [%expect
    {|
    └── Node
        ├── Node
        │   ├── Node
        │   │   ├── Leaf a
        │   │   └── Leaf a
        │   └── Node
        │       ├── Leaf a
        │       └── Leaf a
        └── Node
            ├── Node
            │   ├── Leaf a
            │   └── Leaf a
            └── Node
                ├── Leaf a
                └── Leaf a |}]
;;
