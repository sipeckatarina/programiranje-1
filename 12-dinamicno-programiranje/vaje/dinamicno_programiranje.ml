
(* ===== Vaja: Dinamično programiranje  ===== *)

(* Požrešna miška se nahaja v zgornjem levem kotu šahovnice. Premikati se sme
   samo za eno polje navzdol ali za eno polje na desno in na koncu prispeti v
   desni spodnji kot. Na vsakem polju šahovnice je en sirček. Ti sirčki imajo
   različne (pozitivne) mase. Miška bi se rada kar se da nažrla, zato jo zanima,
   katero pot naj ubere.

   Napišite funkcijo "max_cheese cheese_matrix", ki dobi matriko z
   masami sirčkov in vrne največjo skupno maso, ki jo bo miška požrla, če gre po
   optimalni poti.

   ----------
   # max_cheese cheese_matrix;;
   - : int = 13
   ----------*)

let test_matrix = [| [| 1 ; 2 ; 0; 5; 2; 9 |];
                     [| 2 ; 4 ; 5; 5; 5; 1 |];
                     [| 7 ; 0 ; 1; 5; 2; 9 |];
                     [| 1 ; 2 ; 0; 3; 2; 3 |];
                     [| 2 ; 4 ; 5; 5; 7; 9 |];
                     [| 7 ; 0 ; 1; 5; 2; 9 |];
                     [| 1 ; 2 ; 0; 5; 1; 6 |];
                     [| 2 ; 4 ; 5; 7; 8; 9 |];
                     [| 7 ; 0 ; 1; 5; 2; 9 |]  |]

let memoiziraj_rec odviti_f =
  let rezultati = Hashtbl.create 512 in
  let rec mem_f x =
    if Hashtbl.mem rezultati x then
      Hashtbl.find rezultati x
    else 
      let y = odviti_f mem_f x in
      Hashtbl.add rezultati x y;
      y
  in
  mem_f

let max_cheese cheese_matrix = 
  let max_i = Array.length cheese_matrix in
  let max_j = Array.length cheese_matrix.(0) in
  let rec max_cheese1 recursive_max_cheese (i, j) =
    if i >= max_i || j >= max_j then 0
    else 
      let right = max_cheese1 (i, j+1) in
      let down = max_cheese1 (i+1, j) in
      let our_cheese = cheese_matrix.(i).(j) in
      our_cheese + max right down 
  in
  let memoised_max_cheese = memoiziraj_rec max_cheese1 in
  memoised_max_cheese (0, 0)

(* ne delajo stvari *)

(* Rešujemo problem stolpov, ko smo ga spoznali na predavanjih.
   Imamo štiri različne tipe gradnikov, dva modra in dva rdeča.
   Modri gradniki so višin 2 in 3, rdeči pa višin 1 in 2.

   Napiši funkcijo "alternating_towers height", ki za podano višino "height"
   izpiše število različnih stolpov podane višine, kjer se barva gradnikov
   izmenjuje (rdeč na modrem, moder na rdečem itd.).

   Namig: Uporabi dve pomožni funkciji. Za medsebojno rekurzijo uporabi
          ukaz "and".
   ----------
   # alternating_towers 10;;
   - : int = 35
   ---------- *)

let alternating_towers height = ()
