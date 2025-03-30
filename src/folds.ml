(* Здесь я поигрался со свёртками списка, переписав их в CPS с разными условиями *)

(* Обычная реализация левой свёртки, уже tailrec *)
let rec fold_left f accu l =
  match l with
  | [] -> accu
  | hd :: tl -> fold_left f (f accu hd) tl
;;

(* CPS реализация левой свёртки, f обычная (не в CPS) *)
let fold_left_cps1 f accu l =
  let rec helper xs k =
    match xs with
    | [] -> k accu
    | hd :: tl -> helper tl (fun acc -> k (f (f accu hd) acc))
  in
  helper l Fun.id
;;

(* CPS реализация левой свёртки, f записана в CPS *)
let fold_left_cps2 f accu l =
  let rec helper xs acc k =
    match xs with
    | [] -> k acc
    | hd :: tl -> f acc hd (fun new_acc -> helper tl new_acc k)
  in
  helper l accu Fun.id
;;

(* Обычная реализация правой свёртки, не tailrec *)
let rec fold_right f l accu =
  match l with
  | [] -> accu
  | a :: l -> f a (fold_right f l accu)
;;

(* CPS реализация правой свёртки, f обычная (не в CPS) *)
let fold_right_cps1 f l accu =
  let rec helper xs k =
    match xs with
    | [] -> k accu
    | hd :: tl -> helper tl (fun acc -> k (f hd acc))
  in
  helper l Fun.id
;;

(* CPS реализация правой свёртки, f записана в CPS *)
let fold_right_cps2 f l accu =
  let rec helper xs k =
    match xs with
    | [] -> k accu
    | hd :: tl -> helper tl (fun acc -> f hd acc k)
  in
  helper l Fun.id
;;

(* На какой-то из попыток сдачи 2024 нужно было останавливать вычисления в разных случаях. Например встретили 0 при свёртке умножением. Это пока под вопросом *)

let%expect_test "Default fold_left" =
  print_int (fold_left ( + ) 0 [ 1; 2; 3 ]);
  [%expect {| 6 |}]
;;

let%expect_test "CPS fold_left, f is default" =
  print_int (fold_left_cps1 ( + ) 0 [ 1; 2; 3 ]);
  [%expect {| 6 |}]
;;

let%expect_test "CPS fold_left, f in CPS" =
  let f_cps x y k = k (x + y) in
  print_int (fold_left_cps2 f_cps 0 [ 1; 2; 3 ]);
  [%expect {| 6 |}]
;;

let%expect_test "Default fold_right" =
  print_int (fold_right ( + ) [ 1; 2; 3 ] 0);
  [%expect {| 6 |}]
;;

let%expect_test "CPS fold_right, f is default" =
  print_int (fold_right_cps1 ( + ) [ 1; 2; 3 ] 0);
  [%expect {| 6 |}]
;;

let%expect_test "CPS fold_right, f in CPS" =
  let f_cps x y k = k (x + y) in
  print_int (fold_right_cps2 f_cps [ 1; 2; 3 ] 0);
  [%expect {| 6 |}]
;;
