
(* обычный fold_left *)
let rec fold_left f acc l =
  match l with 
  | [] -> acc
  | hd :: tl -> fold_left f (f hd acc) tl

(* cps fold left*)

let rec fold_left_cps1 f accu l k =
  match l with 
  | [] -> k accu
  | hd :: tl -> fold_left_cps1 f (f hd accu) tl k

(*cps fold left with cps f*)

let fold_left_cps2 f accu l =
  let rec helper xs acc k =
    match xs with
    | [] -> k acc
    | hd :: tl -> f hd acc (fun new_acc -> helper tl new_acc k)
  in
  helper l accu Fun.id


(* обычный fold_right*)
let rec fold_right f l acc =
  match l with 
  | [] -> acc
  | hd :: tl -> f hd (fold_right f tl acc)

(* cps fold_right*)
let fold_right_cps1 f l accu =
  let rec helper xs k =
    match xs with
    | [] -> k accu
    | hd :: tl -> helper tl (fun acc -> k (f hd acc))
  in
  helper l Fun.id
;;

let rec fold_right_cps2 f l accu k =
    match l with 
    | [] -> k accu
    | hd :: tl -> fold_right_cps2 f tl accu (fun res_tail -> f hd res_tail k)

let fold_right_cps2_wh f l accu = 
    let rec helper xs k = 
        match xs with
        | [] -> k accu
        | hd :: tl -> helper tl (fun res_tail -> f hd res_tail k)
    in 
    helper l Fun.id
;;

(*Наблюдение: можно делать через helper, можно без него. Как упражнение, можете
переписать то, что написано через helper, без него*)


(* конкатенация *)
let f x s = s ^ string_of_int x
  (* конкатенация cps *)
let fk x s k =
  (fun t -> k(s ^ t)) (string_of_int x)

let s x y = (x + y)
let sk x y k = k (x + y)

  let list = [1; 2; 3; 4]
  let huge_list = List.init 1_000_000_0 (fun _ -> 1)

  let print_list_n ~print_element n list =
    let rec take n = function
      | [] -> []
      | _ when n <= 0 -> []
      | x :: xs -> x :: take (n - 1) xs in
    List.iter (fun x -> print_element x; print_string " ") (take n list);
    print_newline ()
  ;;

(* проверяем, что фолд в нужную сторону*)
let%expect_test "fold left list" = 
  (try
    print_string  (fold_left f "" list)
  with | Stack_overflow -> print_endline "Stack overflow!");
  [%expect {| 1234 |}] 

  let%expect_test "fold right list" = 
  (try
    print_string  (fold_right f list "")
  with | Stack_overflow -> print_endline "Stack overflow!");
  [%expect {| 4321 |}] 

  let%expect_test "fold left cps1 list" = 
  (try
    print_string  (fold_left_cps1 f "" list Fun.id)
  with | Stack_overflow -> print_endline "Stack overflow!");
  [%expect {| 1234 |}] 

  let%expect_test "fold right cps1 list" = 
  (try
    print_string  (fold_right_cps1 f list "")
  with | Stack_overflow -> print_endline "Stack overflow!");
  [%expect {| 4321 |}] 

  let%expect_test "fold left cps2 list" = 
  (try
    print_string  (fold_left_cps2 fk "" list)
  with | Stack_overflow -> print_endline "Stack overflow!");
  [%expect {| 1234 |}] 

  let%expect_test "fold right cps2 list" = 
  (try
    print_string  (fold_right_cps2 fk list "" Fun.id )
  with | Stack_overflow -> print_endline "Stack overflow!");
  [%expect {| 4321 |}] 

  let%expect_test "fold right cps2 (with helper) list" = 
  (try
    print_string  (fold_right_cps2_wh fk list "")
  with | Stack_overflow -> print_endline "Stack overflow!");
  [%expect {| 4321 |}] 

  (* проверка на stack overflow*)
   
  let%expect_test "fold left list" = 
  (try
    print_int  (fold_left s 0 huge_list)
  with | Stack_overflow -> print_endline "Stack overflow!");
  [%expect {| 10000000 |}] (* fold left хорош без мам пап и cps'ов*)
  
  let%expect_test "fold right list" = 
  (try
    print_int   (fold_right s huge_list 0)
  with | Stack_overflow -> print_endline "Stack overflow!");
  [%expect {| Stack overflow! |}] 

  let%expect_test "fold left cps1 list" = 
  (try
    print_int   (fold_left_cps1 s 0 huge_list Fun.id)
  with | Stack_overflow -> print_endline "Stack overflow!");
  [%expect {| 10000000 |}] 

  let%expect_test "fold right cps1 list" = 
  (try
    print_int   (fold_right_cps1 s huge_list 0)
  with | Stack_overflow -> print_endline "Stack overflow!");
  [%expect {| 10000000 |}] 

  let%expect_test "fold left cps2 list" = 
  (try
    print_int   (fold_left_cps2 sk 0 huge_list)
  with | Stack_overflow -> print_endline "Stack overflow!");
  [%expect {| 10000000 |}] 

  let%expect_test "fold right cps2 list" = 
  (try
    print_int   (fold_right_cps2 sk huge_list 0 Fun.id )
  with | Stack_overflow -> print_endline "Stack overflow!");
  [%expect {| 10000000 |}] 

