(* задание с офиц пересдачи 16.04.2025 *)
(* Реализуйте функцию cartesian: 'a list -> 'b list -> ('a * 'b) list, которая считает декартово произведение двух
   списков (в произвольном порядке следования элементов). Затем запишите декартово произведение в CPS.
   Если нужно будет воспользоваться функциями из стандартной библиотеки, то они тоже должны быть преъявлены в CPS стиле. *)

(* обычная реализация *)

let cartesian l1 l2 = List.concat (List.map (fun e1 -> List.map (fun e2 -> e1, e2) l2) l1)

(* CPS реализация с вспомогательными функциями *)

let rec mapk f l k =
  match l with
  | [] -> k []
  | h :: tl -> mapk f tl (fun s -> k (f h :: s))
;;

let rec appendk l1 l2 k =
  match l1 with
  | [] -> k l2
  | h :: tl -> appendk tl l2 (fun s -> k (h :: s))
;;

let rec concatk xs k =
  match xs with
  | [] -> k []
  | h :: tl -> concatk tl (fun s1 -> appendk h s1 k)
;;

let cartesiank l1 l2 k =
  mapk (fun e1 -> mapk (fun e2 -> e1, e2) l2 Fun.id) l1 (fun s -> concatk s k)
;;

(* нужны для тестов *)

let print_list_int list =
  List.iter (fun (x1, x2) -> Format.printf "(%d %d) " x1 x2) list;
  print_newline ()
;;

let print_list_int_char list =
  List.iter (fun (x1, x2) -> Format.printf "(%d %c) " x1 x2) list;
  print_newline ()
;;

let print_list_int_string list =
  List.iter (fun (x1, x2) -> Format.printf "(%d %s) " x1 x2) list;
  print_newline ()
;;

(* тесты *)

let%expect_test "CPS cartesian with the same number of elements in int_int list" =
  print_list_int (cartesiank [ 1; 2; 3 ] [ 4; 5; 6 ] Fun.id);
  [%expect {| (1 4) (1 5) (1 6) (2 4) (2 5) (2 6) (3 4) (3 5) (3 6) |}]
;;

let%expect_test "CPS cartesian with the same number of elements in int_char list" =
  print_list_int_char (cartesiank [ 1; 2; 3 ] [ 'a'; 'b'; 'c' ] Fun.id);
  [%expect {| (1 a) (1 b) (1 c) (2 a) (2 b) (2 c) (3 a) (3 b) (3 c) |}]
;;

let%expect_test "CPS cartesian with different number of elements" =
  print_list_int_string (cartesiank [ 1; 2; 3 ] [ "7"; "bunu" ] Fun.id);
  [%expect {| (1 7) (1 bunu) (2 7) (2 bunu) (3 7) (3 bunu) |}]
;;

let%expect_test "CPS cartesian with empty lists" =
  print_list_int_char (cartesiank [] [] Fun.id);
  [%expect {| |}]
;;
