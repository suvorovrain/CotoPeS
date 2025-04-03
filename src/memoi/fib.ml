(*принимает функцию из 'a->'b, возвращает функцию того же типа*)
(* val memoize : ('a -> 'b) -> 'a -> 'b *)
let memoize: ('a -> 'b) -> 'a -> 'b = fun f -> 
  let open Base in

	(* создаём таблицу где будем сохранять результаты *)
  let table = Hashtbl.Poly.create () in
  let new_fun x =
    match Hashtbl.find table x with
		(* нашли в таблице вернём, то что нашли*)
    | Some y -> y
		
		(* не нашли *)
    | None ->
			(* посчитали *)
      let y = f x in
			(* добавили в таблицу *)
      Hashtbl.add_exn table ~key:x ~data:y;
		  (* вернули посчитанное значение *)
      y
  in
  new_fun
  ;;
  
(*принимает функцию которая принимает функцию того же типа('a->'b), и возврашает функцию того же типа*)
(*это мемоизация для открытых рекурсий вся соль в том что *)
let memo_rec : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b =
  fun f ->
  let open Base in
  let table = Hashtbl.Poly.create () in
  let rec new_fun x =
    match Hashtbl.find table x with
    | Some y -> y
    | None ->
      (*мы вот тут передаем в функцию именно new_fun которая уже в дальнейшем будет мемоизировать каждый рекурсивынй вызов
      и у которой в замыкании уже лежит hashmap*)
      let y = f new_fun x in
      Hashtbl.add_exn table ~key:x ~data:y;
      y
  in  
  new_fun
;;

(*функция для замера для наглядности сам придумал поэтому будет работать плохо аххахаахах*)
let time_it f x =
  let start = Sys.time () in
  let _ = f x in
  let finish = Sys.time () in
  (finish -. start);
;;

(*обычная реализация*)
let rec fibo n = if n <= 1 then n else fibo (n-1) + fibo (n-2)

(* открытая рекурсия*)
let fib_open self n = if n <= 1 then 1 else self (n - 1) + self (n - 2)

(* мемоизация открытой рекурсии - должно работать лучше всего*)
let memo_rec_fib = memo_rec fib_open

(*обычная мемоизация*)
let mem_fib = memoize fibo


(*
  видим что реализация мемоизированного фибоначи выполняется больше чем за 10 секунд первый раз
  из теста видно что первый раз функция раотает долго (у меня 45секунд первый запуск и 0.00001 второй)
  но трейтий запуск от числа 49 все равно займет порядка 45 секунд тк результат не был мемоизирован, мемоизированно было только вычисление от 50
let%expect_test "Compare execution time of stupid memoized fibo" =
  let time = time_it mem_fib 50 in
  let reference_time = 10.0 in  (* Максимально допустимое время *)
  Printf.printf "First execution time: %f (reference: %f)\n" time reference_time;
  if time > reference_time then
    Printf.printf "TOO SLOW! execution took more then %.2f)\n"  reference_time
  else
    Printf.printf "Acceptable time: execution took less then <= %.2f)\n" reference_time;
  
  let time2 = time_it mem_fib 50 in
  let reference_time_2 = 1.0 in
  Printf.printf "Second execution time: %f (reference: %f)\n" time2 reference_time_2;
  if time2 > reference_time_2 then
    Printf.printf "TOO SLOW! execution took more then %.2f)\n"  reference_time_2
  else
    Printf.printf "Acceptable time: execution took less then <= %.2f)\n" reference_time_2;
  
  let time3 = time_it mem_fib 49 in
  let reference_time_3 = 1.0 in
  Printf.printf "Third execution time: %f (reference: %f)\n" time3 reference_time_3;
  if time3 > reference_time_3 then
    Printf.printf "TOO SLOW! execution took more then %.2f)\n"  reference_time_3
  else
    Printf.printf "Acceptable time: execution took less then <= %.2f)\n" reference_time_3;  
  
  [%expect {|
    First execution time: 43.579227 (reference: 10.000000)
    TOO SLOW! execution took more then 10.00)
    Second execution time: 0.000002 (reference: 1.000000)
    Acceptable time: execution took less then <= 1.00)
    Third execution time: 26.729995 (reference: 1.000000)
    TOO SLOW! execution took more then 1.00) |}]
;;


во первых можно заметить что первый запуск занял тоже довольно быстро не 45 секунд а 1 сколько то там тысячных, потому что фактически, каждый вызов фибоначи считает две фунции
каждый следующий снова две и снова две и снова две, но благодаря такой мемоизации мы посчитали ТОЛЬКО ОДНУ ветку дерева, сохранили все результаты а дальеш толко к мапе обращались!!!!
let%expect_test "Compare execution time of stupid memoized fibo" =
  let time = time_it memo_rec_fib 50 in
  let reference_time = 10.0 in  (* Максимально допустимое время *)
  Printf.printf "First execution time: %f (reference: %f)\n" time reference_time;
  if time > reference_time then
    Printf.printf "TOO SLOW! execution took more then %.2f)\n"  reference_time
  else
    Printf.printf "Acceptable time: execution took less then <= %.2f)\n" reference_time;
  
  let time2 = time_it memo_rec_fib 50 in
  let reference_time_2 = 1.0 in
  Printf.printf "Second execution time: %f (reference: %f)\n" time2 reference_time_2;
  if time2 > reference_time then
    Printf.printf "TOO SLOW! execution took more then %.2f)\n"  reference_time
  else
    Printf.printf "Acceptable time: execution took less then <= %.2f)\n" reference_time_2;
  
  let time3 = time_it memo_rec_fib 49 in
  let reference_time_3 = 1.0 in
  Printf.printf "Third execution time: %f (reference: %f)\n" time3 reference_time_3;
  if time3 > reference_time_3 then
    Printf.printf "TOO SLOW! execution took more then %.2f)\n"  reference_time_3
  else
    Printf.printf "Acceptable time: execution took less then <= %.2f)\n" reference_time_3;  
  [%expect {|
    First execution time: 0.000010 (reference: 10.000000)
    Acceptable time: execution took less then <= 10.00)
    Second execution time: 0.000001 (reference: 1.000000)
    Acceptable time: execution took less then <= 1.00)
    Third execution time: 0.000001 (reference: 1.000000)
    Acceptable time: execution took less then <= 1.00) |}]
;;


*)



(*тут можно запустить потыкаться, будет видно что 50 впервый раз у всех будет работать минуту,
 затем повторно у мемоизированных будет рабоать за 0~ а вот у обычной опять минуту*)
 (*при этом в третий раз мемоизация на 49 только у оптимизированного мемоийза будет быстро тк простой не сохранял все рекурсивные вызовы*)
(*
let () = 
  Printf.printf "Memoized\n";
  Printf.printf "%d\n" (time_it mem_fib 50);
  Printf.printf "NotMemoized\n";
  Printf.printf "%d\n" (time_it fibo 50);
  Printf.printf "OptaimalMemoized\n";
  Printf.printf "%d\n" (time_it fib_opt 50);

  Printf.printf "Memoized second time\n";
  Printf.printf "%d\n" (time_it mem_fib 50);
  Printf.printf "NotMemoized\n";
  Printf.printf "%d\n" (time_it fibo 50);
  Printf.printf "OptaimalMemoized\n";
  Printf.printf "%d\n" (time_it fib_opt 50);

  Printf.printf "Memoized third time\n "; 
  Printf.printf "%d\n" (time_it mem_fib 49);
  Printf.printf "NotMemoized\n";
  Printf.printf "%d\n" (time_it fibo 49);
  Printf.printf "OptaimalMemoized\n";
  Printf.printf "%d\n" (time_it fib_opt 49);

```
*)

(*факториал*)
let factorial_open self n = 
  if n <= 1 then 1  
  else n * self (n - 1)

let fact_opt = memo_rec factorial_open

(*сумма чисел от одного до n  *)
let sum_open self n =
  if n = 0 then 0
  else n + self (n - 1)

let memo_sum = memo_rec sum_open


(*открытая рекурсия на функцию является ли число простым*)
let is_prime_open self n =
  if n <= 1 then false
  else if n = 2 then true
  else 
    let check i = 
      if i * i > n then true
      else if n mod i = 0 then false
      else self (i + 1)  (* Используем переданную self вместо прямой рекурсии *)
    in
    check 2

let memo_is_prime = memo_rec is_prime_open
