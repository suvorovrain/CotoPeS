(* Реализуйте функцию map для списков, которая получает список xs и функцию f, и применяет функцию к каждому элементу, и
складывает в результат в ОБРАТНОМ порядке. Затем функцию map запишите в CPS. Учтите, что передаваемая функция f тоже может
быть в CPS стиле*)
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

(* обычная мапа, в обратном порядке *)
let rec map f xs acc =
  match xs with
  | [] -> acc
  | hd :: tl -> map f tl (f hd :: acc)
;;

(*Dabzelosiqqq :: сделал обычную мапу которая принимает f не в цпс и так же реверсит список снизу так же есть тест*)
let map_cps_rev f l =
  let rec helper xs k =
    match xs with
    | [] -> k []
    | hd :: tl -> helper tl (fun acc -> k (f hd :: acc))
  in
  helper l Fun.id
;;

(* CPS мапа от МАКСИМА РОДИОНОВА ЖЕСТКОГО ЧЕЛА ну и я сам подумал как перевернуть*)
(*Dabzelosiqqq:::: я сдела разворот без конкатенации списков - он по идее работает медленно так что это тоже самое что и выше только через аккумулятор*)
let map_cps_revk f l =
  let rec helper xs acc k =
    match xs with
    | [] -> k acc
    | hd :: tl -> f hd (fun mapped_hd -> helper tl (mapped_hd :: acc) k)
  in
  helper l [] Fun.id
;;

(* тут важно условие на то, что f в CPS*)
let rec fact x = if x = 1 then 1 else x * fact (x - 1)
let rec factk x k = if x = 1 then k 1 else factk (x - 1) (fun s -> k (s * x))

let%expect_test "default map" =
  print_list ~print_element:print_int (map (fun x -> fact x) [ 2; 4; 6 ] []);
  [%expect {| 720 24 2 |}]
;;

let%expect_test "CPS map" =
  print_list ~print_element:print_int (map_cps_revk (fun x -> factk x) [ 2; 4; 6 ]);
  [%expect {| 720 24 2 |}]
;;

(*милый тестик показывающий что моя функция работает так же и ничем не хуже*)
let%expect_test "CPS map" =
  print_list ~print_element:print_int (map_cps_revk factk [ 2; 4; 6 ]);
  [%expect {| 720 24 2 |}]
;;

(* Слава попросил сделать тест что это чудо что я наваял не перегружает стек чтбы убедиться что оно действиетльно CPS*)
(*запускаем нашу чудо функцию на листе из 10_000_000 единичек, факториалем их все считайте получаем те же 10_000_000 единичек, но стак оверфлоу мы не получили*)
let%expect_test "default map huge list" =
  let huge_list = List.init 10_000_000 (fun _ -> 1) in
  (try print_list_n ~print_element:print_int 5 (map fact huge_list []) with
   | Stack_overflow -> print_endline "Stack overflow!");
  [%expect {| 1 1 1 1 1 |}]
;;

let%expect_test "cps map handles large input safely" =
  let huge_list = List.init 10_000_000 (fun _ -> 1) in
  let result = map_cps_revk factk huge_list in
  print_int (List.length result);
  [%expect {| 10000000 |}]
;;

(*тест что функция map_cps_rev реверсит список на небольшом списке*)
let%expect_test "default cps map huge list" =
  (try
     print_list_n ~print_element:print_int 5 (map_cps_rev (fun x -> fact x) [ 2; 4; 6 ])
   with
   | Stack_overflow -> print_endline "Stack overflow!");
  [%expect {| 2 24 720 |}]
;;

(*тест что функция map_cps_rev не падает  на огромном списке*)
let%expect_test "default cps map huge list" =
  let huge_list = List.init 10_000_000 (fun _ -> 1) in
  (try
     print_list_n ~print_element:print_int 5 (map_cps_rev (fun x -> fact x) huge_list)
   with
   | Stack_overflow -> print_endline "Stack overflow!");
  [%expect {| 1 1 1 1 1 |}]
;;
