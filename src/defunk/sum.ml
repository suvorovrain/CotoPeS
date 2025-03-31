let rec sum1 xs n =
  match xs with
  | [] -> apply n 0
  | h :: tl -> sum1 tl (n+h)
 and apply f x = f + x