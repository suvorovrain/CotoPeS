(* Упражнение 2 с лекции про CPS. Описать API из функций start, fin, push, add, mul, чтобы писать код например
так:
start push 1 push 2 add push 3 mul fin
И выражение будет иметь тип int и вычисляться (в польской нотации, стеком) в 9 *)
(* Код от Карима *)
let start f = f [] (* просто передали пустой список, имитирующий стек, в продолжение *)
let push stack x f = f (x :: stack)
(* добавили значение к голове списка ("стека"), первый push получит его как аргумент от start *)

let add stack f =
  match stack with
  | x :: y :: rest ->
    f ((x + y) :: rest) (* сняли два элемента со стека, сложили на стек их сумму *)
  | _ -> failwith "Not enough elements on the stack for addition"
;;

let mul stack f =
  match stack with
  | x :: y :: rest ->
    f ((x * y) :: rest) (* сняли два элемента со стека, сложили на стек их произведение *)
  | _ -> failwith "Not enough elements on the stack for multiplication"
;;

let fin stack =
  match stack with
  | [ result ] ->
    result (* если на стеке лишь один элемент, то он и будет ответом в итоге *)
  | _ -> failwith "Invalid stack state at the end of computation"
;;

let%expect_test "Eval expression" =
  print_int (start push 1 push 2 add push 3 mul fin);
  [%expect {| 9 |}]
;;

let sum_n_ones n =
  let rec build_ops n ops =
    if n <= 0 then start ops else build_ops (n - 1) (fun stack -> push stack 1 ops)
  in
  let rec add_ops count ops =
    if count <= 0 then ops else add_ops (count - 1) (fun stack -> add stack ops)
  in
  build_ops n (add_ops (n - 1) fin)
;;

let%expect_test "Eval huge expression" =
  (try Printf.printf "Success! Sum: %d\n" (sum_n_ones 1_000_000) with
   | Stack_overflow -> print_endline "Stack overflow!");
  [%expect {| Success! Sum: 1000000 |}]
;;
