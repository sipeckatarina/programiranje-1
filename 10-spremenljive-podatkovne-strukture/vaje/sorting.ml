(* Generate a list of length len with random values up to max. Example:
utop[1]> let l = randlist 10 10 ;;
val l : int list = [0; 1; 0; 4; 0; 9; 1; 2; 5; 4]

Can be used to test sorting functions as in
let l = (randlist 100 100) in selection_imperative_list l = List.sort compare l;;

 *)
let rec randlist len max =
  match len with
  | 0 -> []
  | _ -> (Random.int max) :: randlist (len-1) max

(* The function insert y xs inserts y into the already sorted list xs and
   returns a sorted list.

   For example:

utop[75]> insert 9 [0; 2];;
- : int list = [0; 2; 9]
utop[76]> insert 1 [0; 2];;
- : int list = [0; 1; 2]
utop[79]> insert 1 [];;
- : int list = [1]
 *)

let rec insert l y  =
  let rec ins y acc = function
  | [] -> List.rev (y :: acc) 
  | x :: xs when x <= y -> ins y (x :: acc) xs
  | x :: xs as t -> (List.rev acc) @ (y :: t)
  in ins y [] l

(* The empty list is sorted. We can sort a list by consecutively inserting all
   of its elements into the empty list. Use this idea to write insertion_sort
   using List.fold_left and insert. *)

let rec ins_sort l = 
  let rec in_sort acc = function
  | [] -> acc
  | x :: xs -> in_sort (insert acc x) xs
  in 
  in_sort [] l 

let rec ins_sort2 l =
  List.fold_left insert [] l

(* Write a recursive function that takes a list l and if l is non-empty,
   returns a pair Some (z, l_without_z) such that z is the smallest element in
   l and l_without_z is l with the first occurance of z removed. *)

let rec find_min l =
  let rec find_min' = function
  | [] -> 0 
  | x :: xs -> x
  in 
  find_min' (List.fold_left insert [] l)


let rec min_and_rest l = 
  let the_minimum =  find_min l in
  let rec min' acc = function
    | [] -> None
    | x :: xs when x = the_minimum -> Some (x, (List.rev acc) @ xs)
    | x :: xs -> min' (x :: acc) xs 
  in min' [] l


(* Selection sort works by keeping a list l partitioned into a sublist that is
   already sorted and a sublist of left-overs that still needs treatment, and
   consecutively moving the smallest element of the left-overs to the sorted
   part. *)

(* Use min_and_rest to implement selection sort as a recursive function. *)

let rec selection_sort l = 
  let rec selection_sort' acc = function
    | None -> List.rev acc
    | Some(x, li) -> selection_sort' (x :: acc) (min_and_rest li) 
  in 
  (selection_sort' [] (min_and_rest l)) 


(* When working with arrays instead of lists, selection sort can work
   "in-place", i.e. without creating intermediate copies of (parts of) the
   input. it still works by partitioning the input into a sorted part and a
   to-do part. The sorted part is always an initial segment of the input array,
   delimited by an index boundary_sorted. To make progress, we don't extract
   the smallest element of the left-overs, but locate its index, and then swap
   it with the element located at the boundary. When the boundary reaches the
   end of the array, the input is sorted. *)

(* Write a function swap a i j that exchanges a.(i) and a.(j) *)

let swap a i j = failwith "todo"

(* Write a function index_min a lower upper that computes the index of the
   smallest element in a between indices lower and upper. Example:
   index_min [|0; 2; 9; 3; 6|] 2 4 = 4 *)
let index_min a lower upper = failwith "todo"

(* Implement in-place selection sort. *)
let selection_imperative a = failwith "todo"

(* To test your array-function, you can turn it into a function that sorts
   lists by using Array.of_list and Array.to_list. *)
let selection_imperative_list l = failwith "todo"
