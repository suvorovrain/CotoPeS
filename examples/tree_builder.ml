(* помните мультик боб строитель? *)
(* так вот это цпс для строителя деревьев. *)
type tree =
| Leaf
| Node of tree * tree

let print_clean_vertical tree =
  let rec print_node indent is_last = function
    | Leaf -> 
        Printf.printf "%s%sLeaf\n" indent (if is_last then "\\_" else "| ")
    | Node (l, r) ->
        Printf.printf "%s%sNode\n" indent (if is_last then "\\_" else "| ");
        let new_indent = indent ^ (if is_last then "  " else "| ") in
        print_node new_indent false l;
        print_node new_indent true r
  in
  print_node "" true tree

(* в оригинале тут было дерево, которое разделялось через каждые 1000000 узлов, но mod в цпс переписывать не хочу, поэтому depth = 2  *)
let rec make depth =
  if depth <= 0 then Leaf
  else 
    let r = make (depth - 1) in
    let l = if depth = 2 then r else Leaf in
    Node (l,r)

let size root = 
  let rec helper tree = 
  match tree with
    | Leaf -> 0
    | Node (l, r) -> 1 + helper l + helper r
    in helper root

(* CPS *)

(* тут просто взяли и расписали let через применение. 025pacomb.pdf слайд 14 *)
(* можно ввести let* и переписать с ними *)
let rec makek =
  (fun depth k ->
  if depth <= 0 then k Leaf
  else 
    makek (depth-1) (fun r -> (fun l -> k (Node (l,r))) (if depth = 2 then r else Leaf) ))


let rec sizek root k = 
  match root with
  | Leaf -> k 0
  | Node(l,r) -> sizek l (fun s1 -> sizek r (fun s2 -> k (s1 + s2 + 1)))


let%expect_test _ = print_clean_vertical (make 4);
[%expect{|
  \_Node
    | Leaf
    \_Node
      | Leaf
      \_Node
        | Node
        | | Leaf
        | \_Leaf
        \_Node
          | Leaf
          \_Leaf |}]


let%expect_test _ = print_int (size (make 4));
[%expect{|
  5 |}]
let%expect_test _ = print_int (size (makek 4 Fun.id));
[%expect{|
  5 |}]
