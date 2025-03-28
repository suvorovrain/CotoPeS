(* factorial *)
let rec fact x = if x = 1 then 1 else x * fact (x-1)

let rec factk x k = if x = 1 then k 1 else factk (x-1) (fun s -> k (s* x))

let%expect_test _ = print_int (factk 5 (fun x -> x));
  [%expect {| 120 |}]
let%expect_test _ = print_int (fact 5);
  [%expect {| 120 |}]

