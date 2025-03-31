(*обычный фибоначи*)
let rec fib n = if n <= 1 then 1 else fib(n-1) + fib(n-2)

(* CPS (мега хайп) fibonacci*)

let rec fibk n k = if n <= 1 then k 1 else fibk (n-1) (fun s1 -> fibk (n-2) (fun s2 -> k (s1 + s2))) 

let%expect_test "Common Fibonacci" = print_int (fib 5);
[%expect{| 8 |}]

let%expect_test "Sigma Fibonacci" = print_int (fibk 30 Fun.id);
[%expect{| 1346269 |}]

let%expect_test "Common Fibonacci on big number" =
  (try print_int (fib 10000000) with
  | Stack_overflow ->
    print_endline "Stack overflow!");
    [%expect {| Stack overflow! |}]
;;