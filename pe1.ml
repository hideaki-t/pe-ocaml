(* *)
let rec range a b = if a == b then [a] else a :: range (a + 1) b;;

let rec filter f l =
  match l with
    [] -> []
  | x :: xs ->
     if f x then x :: filter f xs
     else filter f xs
;;

let rec fold_left f i l =
  match l with
    [] -> i
  | x :: xs -> fold_left f (f i x) xs
;;

print_int (fold_left (+) 0 (filter (fun x -> (x mod 3 == 0) || (x mod 5 == 0)) (range 1 999)));
print_newline ();;

print_int (List.fold_left (+) 0 (List.filter (fun x -> (x mod 3 == 0) || (x mod 5 == 0)) (range 1 999)));
print_newline ();;
