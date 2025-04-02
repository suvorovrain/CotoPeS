(* val memoize : ('a -> 'b) -> 'a -> 'b *)
let memoize f =
  let table =
    Hashtbl.create 42
    (* size *)
  in
  fun key ->
    match Hashtbl.find table key with
    | rez -> rez
    | exception Not_found ->
      let rez = f key in
      Hashtbl.add table key rez;
      rez
;;

(* val memoize2 : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b *)
let memoize2 f =
  let h =
    Hashtbl.create 42
    (* size *)
  in
  let rec g x =
    match Hashtbl.find h x with
    | rez -> rez
    | exception Not_found ->
      let rez = f g x in
      Hashtbl.add h x rez;
      rez
  in
  g
;;

(* let rec fib n =
  if n<2 then n else fib (n-1) + fib (n-2) ;; *)
let fib_open self n = if n <= 1 then 1 else self (n - 1) + self (n - 2)
let rec fib n = fib_open fib n
let fib = memoize fib
let fib2 = memoize2 fib_open
let res1 = fib 30 (* slow *)
let res2 = fib2 30 (* faster *)
