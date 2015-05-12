let rec fib a b limit =
  if a >= limit then []
  else a :: fib b (a + b) limit
;;

print_int (List.fold_left (+) 0  (List.filter (fun x -> (x mod 2 == 0)) (fib 1 2 4000000)));;
print_newline ();;


let rec fib a b limit l =
  if a >= limit then l
  else fib b (a + b) limit (a::l)
;;

print_int (List.fold_left (+) 0  (List.filter (fun x -> (x mod 2 == 0)) (fib 1 2 4000000 [])));
print_newline ();;


type fibseq = Cons of ((int * int) * (int * int -> fibseq));;
let rec fibgen (a, b) = Cons((b, a + b), fibgen);;
let rec takewhile limit (Cons ((a, b), f)) =
  if a >= limit then [] else a :: (takewhile limit (f(a, b)));;
print_int (List.fold_left (+) 0 (List.filter (fun x -> (x mod 2 == 0)) (takewhile 4000000 (fibgen(1, 1)))));;
print_newline ();
