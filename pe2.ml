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


type 'a seq = Nils | Cons of 'a * ('a -> 'a seq);;
type 'a infseq = Cons of 'a * ('a -> 'a infseq);;
let rec fibgen (a, b) = Cons((b, a + b), fibgen);;
let rec takewhile limit vf = function
    Cons(a, f) -> if vf a >= limit then [] else (vf a) :: (takewhile limit vf (f a));;
print_int (List.fold_left (+) 0 (List.filter (fun x -> (x mod 2 == 0)) (takewhile 4000000 (fun (a,b) -> a) (fibgen(1, 1)))));;
print_newline ();
