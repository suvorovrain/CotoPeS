(* factorial *)

(* обычная реализация факториала, не хвосто-рекурсивная *)
let rec fact x = if x = 1 then 1 else x * fact (x - 1)

(* реализация факториала в CPS-стиле *)
let rec factk x k = if x = 1 then k 1 else factk (x - 1) (fun s -> k (s * x))

let%expect_test "Simple factorial" =
  print_int (fact 5);
  [%expect {| 120 |}]
;;

let%expect_test "Factorial CPS" =
  print_int (factk 5 Fun.id);
  [%expect {| 120 |}]
;;

let%expect_test "Simple factorial on big number" =
  (try print_int (fact 10000000) with
   | Stack_overflow -> print_endline "Stack overflow!");
  [%expect {| Stack overflow! |}]
;;
