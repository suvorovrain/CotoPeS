(* Реализуйте функцию map для списков, которая получает список xs и функцию f, и применяет функцию к каждому элементу, и
складывает в результат по порядку. Затем функцию map запишите в CPS. Учтите, что передаваемая функция f тоже может быть в
CPS стиле.*)
let print_list ~print_element list =
  List.iter
    (fun x ->
       print_element x;
       print_string " ")
    list;
  print_newline ()
;;

let print_list_n ~print_element n list =
  let rec take n = function
    | [] -> []
    | _ when n <= 0 -> []
    | x :: xs -> x :: take (n - 1) xs
  in
  List.iter
    (fun x ->
       print_element x;
       print_string " ")
    (take n list);
  print_newline ()
;;

(* обычная мапа *)
let rec map f xs =
  match xs with
  | [] -> []
  | hd :: tl -> f hd :: map f tl
;;

(*Dabzelos: мапа в cps которая принимает обычную функцию*)
let map_cps f l =
  let rec helper xs k =
    match xs with
    | [] -> k []
    | hd :: tl -> helper tl (fun r -> k (f hd :: r))
  in
  helper l Fun.id
;;

(* CPS мапа от МАКСИМА РОДИОНОВА ЖЕСТКОГО ЧЕЛА*)
let mapk f l =
  let rec helperk xs k =
    match xs with
    | [] -> k []
    | hd :: tl -> f hd (fun s1 -> helperk tl (fun s2 -> k (s1 :: s2)))
  in
  helperk l Fun.id
;;

(* тут важно условие на то, что f в CPS*)
let rec fact x = if x = 1 then 1 else x * fact (x - 1)
let rec factk x k = if x = 1 then k 1 else factk (x - 1) (fun s -> k (s * x))

let%expect_test "default map" =
  print_list ~print_element:print_int (map (fun x -> fact x) [ 2; 4; 6 ]);
  [%expect {| 2 24 720 |}]
;;

let%expect_test "CPS map" =
  print_list ~print_element:print_int (mapk (fun x -> factk x) [ 2; 4; 6 ]);
  [%expect {| 2 24 720 |}]
;;

let huge_list = List.init 1_000_000 (fun _ -> 1)

let%expect_test "default map huge list" =
  (try print_list_n ~print_element:print_int 5 (map (fun x -> fact x) huge_list) with
   | Stack_overflow -> print_endline "Stack overflow!");
  [%expect {| Stack overflow! |}]
;;

let%expect_test "CPS map huge list" =
  (try print_list_n ~print_element:print_int 5 (mapk (fun x -> factk x) huge_list) with
   | Stack_overflow -> print_endline "Stack overflow!");
  [%expect {| 1 1 1 1 1 |}]
;;

(*тут видно что обычный CPS который работает с обычной f тоже справляется и ведет себя так же*)
let%expect_test "default cps map huge list" =
  (try print_list_n ~print_element:print_int 5 (map_cps (fun x -> fact x) huge_list) with
   | Stack_overflow -> print_endline "Stack overflow!");
  [%expect {| 1 1 1 1 1 |}]
;;
