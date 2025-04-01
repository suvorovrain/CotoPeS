(* Реализуйте функцию map для списков, которая получает список xs и функцию f, и применяет функцию к каждому элементу, и
складывает в результат в ОБРАТНОМ порядке. Затем функцию map запишите в CPS. Учтите, что передаваемая функция f тоже может
быть в CPS стиле*)
let print_list ~print_element list =
  List.iter (fun x -> print_element x; print_string " ") list;
  print_newline ()

(* обычная мапа, в обратном порядке *)
let rec map f xs acc =
  match xs with
  | [] -> acc
  | hd :: tl -> map f tl (f hd :: acc)

(* CPS мапа от МАКСИМА РОДИОНОВА ЖЕСТКОГО ЧЕЛА ну и я сам подумал как перевернуть*)

let mapk f l  =
  let rec helperk xs k  =
    match xs with 
    | [] -> k []
    | hd :: tl -> helperk tl (fun s1 -> f hd (fun s2 -> k (s1 @ [s2]))) 
  in helperk l Fun.id
(* тут важно условие на то, что f в CPS*)
let rec fact x = if x = 1 then 1 else x * fact (x - 1)
let rec factk x k = if x = 1 then k 1 else factk (x - 1) (fun s -> k (s * x))

let%expect_test "default map" = print_list ~print_element:print_int (map (fun x -> fact x) [2;4;6] [] );
[%expect{| 720 24 2 |}]

let%expect_test "CPS map" = print_list ~print_element:print_int (mapk ( (fun x -> factk x)) [2;4;6] );
[%expect{| 720 24 2 |}]
