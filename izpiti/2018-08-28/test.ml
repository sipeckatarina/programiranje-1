(* __________ 1. naloga ____________*)

let rec razlika_kvadratov m n = (m * m + n * n) - (m + n) * (m + n)

let rec uporabi_na_paru f (x, y) = (f x, f y)

let rec ponovi_seznam n sez =
  match n with 
    | n when n <= 0 -> []
    | n -> sez @ (ponovi_seznam (n-1) sez)

let rec razdeli sez =
  let rec raz neg_acc poz_acc = function
    | [] -> (List.rev neg_acc, List.rev poz_acc)
    | x :: xs -> 
      if x < 0 then raz (x :: neg_acc) poz_acc xs
      else raz neg_acc (x :: poz_acc) xs
  in
  raz [] [] sez 

(* __________ 2. naloga ____________*)

type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree

let narascajoca_pot tr = ()

let narascajoca_pot tr = ()

let rec monotona_pot tr = ()


(* __________ 3. naloga ____________*)

type 'a veriga = 
  | Filter of ('a -> bool) * 'a list * 'a veriga
  | Ostalo of 'a list

let test = 
  Filter ((fun x -> x < 0), []),
  Filter ((fun x -> x < 10), []),
  Ostalo ([])