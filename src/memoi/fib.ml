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


let rec fibo n = 
  if n <= 1 then n 
  else fibo (n-1) + fibo (n-2)
  
(*функция для замера для наглядности сам придумал*)
let time_it f x =
  let start = Sys.time () in
  let result = f x in
  let finish = Sys.time () in
  Printf.printf "Execution time: %fs\n" (finish -. start);
  result
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

let fib_open self n = 
  if n <= 1 then 1 
  else self (n - 1) + self (n - 2)


let fib_opt = memo_rec fib_open

let mem_fib = memoize fibo in



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

(*проверка числа на простоту*)
let is_prime_open self n =
  if n <= 1 then false
  else if n = 2 then true
  else 
    let rec check i = 
      if i * i > n then true
      else if n mod i = 0 then false
      else check (i + 1)
    in
    check 2

let memo_is_prime = memo_rec is_prime_open

