(* задание с досрочной пересдачи 7.04.2025 *)
(* Напишите операцию bind для монады "список". Затем эту функцию перепишите в CPS.
   Если в реализации используются рекурсивные функции из стандартной библиотеки, то они тоже должны быть в CPS стиле *)

module type MONADLIST = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val fail : string -> 'a t
end

(* Обычная реализация *)

module ListMonad : MONADLIST with type 'a t = 'a list = struct
  type 'a t = 'a list

  let return x = [ x ]
  let ( >>= ) m f = List.concat (List.map f m)
  let fail _ = []
end

(* CPS реализация *)

module ListCPS = struct
  type 'a t = 'a list

  let return x = [ x ]

  let rec mapk f m k =
    match m with
    | [] -> k []
    | h :: tl -> mapk f tl (fun s -> k (f h :: s))
  ;;

  let rec concatk xs k =
    match xs with
    | [] -> k []
    | h :: tl -> concatk tl (fun s -> k (h @ s))
  ;;

  let bindk m f k = mapk f m (fun s -> concatk s k)
  let fail _ = []
end

(* вспомогательные функции *)

let print_int_list list =
  List.iter (fun x -> Format.printf "%d " x) list;
  print_newline ()
;;

let print_char_list list =
  List.iter (fun x -> Format.printf "%c " x) list;
  print_newline ()
;;

(* тесты *)

let%expect_test "CPS map" =
  print_int_list (ListCPS.bindk [ 1; 2; 3 ] (fun x -> [ x * 2 ]) Fun.id);
  [%expect {| 2 4 6 |}]
;;

let%expect_test "CPS map" =
  print_char_list (ListCPS.bindk [ 67; 80; 83 ] (fun x -> [ char_of_int x ]) Fun.id);
  [%expect {| C P S |}]
;;
