(* Упражнение 2 с лекции про CPS. Описать API из функций start, fin, push, add, mul, чтобы писать код например
так:
start push 1 push 2 add push 3 mul fin
И выражение будет иметь тип int и вычисляться (в польской нотации, стеком) в 9 *)
(* Код от Карима *)
let start f = f []
let push stack x f = f (x :: stack)

let add stack f =
  match stack with
  | x :: y :: rest -> f ((x + y) :: rest)
  | _ -> failwith "Not enough elements on the stack for addition"
;;

let mul stack f =
  match stack with
  | x :: y :: rest -> f ((x * y) :: rest)
  | _ -> failwith "Not enough elements on the stack for multiplication"
;;

let fin stack =
  match stack with
  | [ result ] -> result
  | _ -> failwith "Invalid stack state at the end of computation"
;;

let%expect_test "Eval expression" =
  print_int (start push 1 push 2 add push 3 mul fin);
  [%expect {| 9 |}]
;;
