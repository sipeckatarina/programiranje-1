(* ========== Exercise 3: Types  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 When modeling money, we usually use floats. However we quickly run into
 problems when currency is introduced.
 We shall look at two attempts to improve safety when using currency.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Define the types [euro] and [dollar], where each has only one constructor,
 which accepts a float.
 Then define the functions [euro_to_dollar] and [dollar_to_euro] which convert
 between the two currencies (get the correct factors online or make them up).

 Hint: Marvel at how informative the types are.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dollar_to_euro;;
 - : dollar -> euro = <fun>
 # dollar_to_euro (Dollar 0.5);;
 - : euro = Euro 0.4305
[*----------------------------------------------------------------------------*)

type euro = Euro of float
type dollar = Dollar of float

let dollar_to_euro_fuj d = 0.2 *. d

let dollar_to_euro d = function
  | Dollar v -> Euro (0.2 *. v)


(*----------------------------------------------------------------------------*]
 Define the type [currency] as a single variant type with constructors for the
 currencies yen, pound and krona. Then define the function [to_pound], which
 converts the given currency to the pound.

 Hint: Additionally add the franc as a currency and get excited over the fact
   that Ocaml reminds you to correct the function [to_pound].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # to_pound (Yen 100.);;
 - : currency = Pound 0.007
[*----------------------------------------------------------------------------*)

type currency = 
  | Yen of float
  | Pound of float
  | Krona of float

let to_pound = function
  | Yen v -> Pound (2. *. v)
  | Pound v -> Pound v
  | Krona v -> Pound (0.36 *. v)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 We wish to use lists that keep integers as well as booleans. This can be
 solved by introducing a type of integer or boolean values, however we will
 instead introduce a new type for lists.

 Recall that the type [list] uses a constructor for the empty list [Nil]
 (or [] in Ocaml) and a constructor for an element [Cons(x, xs)] (or x :: xs in
 Ocaml).
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Define the type [intbool_list] with constructors for:
  1.) the empty list,
  2.) an integer element,
  3.) a boolean element.

 Define an example, which represents "[5; true; false; 7]".
[*----------------------------------------------------------------------------*)

type intbool_list = 
	| Empty
	| Non_empty of int * intbool_list
	| Non_empty2 of bool * intbool_list

(*----------------------------------------------------------------------------*]
 The function [intbool_map f_int f_bool ib_list] maps the values of [ib_list]
 into a new [intbool_list] using the appropriate function out of [f_int] and
 [f_bool].
[*----------------------------------------------------------------------------*)

let fint = function
	| i -> 4 * i

let fbool = function
	| true -> false
	| false -> true

let rec intbool_map (f_int : int -> int) (f_bool : bool -> bool) = function
	| Empty -> Empty
	| Non_empty(x, xs) -> Non_empty(f_int x, intbool_map f_int f_bool xs)
	| Non_empty2(x, xs) -> Non_empty2(f_bool x, intbool_map f_int f_bool xs)


(*----------------------------------------------------------------------------*]
 The function [intbool_reverse] reverses the order of elements of an
 [intbool_list]. The function is tail-recursive.
[*----------------------------------------------------------------------------*)

let rec intbool_reverse list = 
	let rec intbool_reverse' (acc : intbool_list) = function
		| Empty -> acc
		| Non_empty(glava, rep) -> intbool_reverse' (Non_empty(glava, acc)) rep
		| Non_empty2(glava, rep) -> intbool_reverse' (Non_empty2(glava, acc)) rep
	in 
	intbool_reverse' Empty list

(*----------------------------------------------------------------------------*]
 The function [intbool_separate ib_list] separates the values of [ib_list] into
 a pair of regular [list] lists, where the first one includes all integers and
 the second one all boolean values. The function is tail-recursive and does not
 change the order of elements.
[*----------------------------------------------------------------------------*)

let rec intbool_separate list = 
	let rec intbool_separate' (acc1 : intbool_list) (acc2 : intbool_list) = function
		| Empty -> (acc1, acc2)
		| Non_empty(glava, rep) -> intbool_separate' (Non_empty(glava, acc1)) (acc2) rep
		| Non_empty2(glava, rep) -> intbool_separate' (acc1) (Non_empty2(glava, acc2)) rep
	in
	intbool_separate' Empty Empty list 

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 You were chosen to be the database administrator for a world renowned wizard
 university "Effemef". Your task is to construct a simple system for data
 management.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Wizards are classified according to their chosen school of magic. Define a
 type [magic] which includes the magic of fire, frost and arcane.

 After being employed, a wizard can decide to be a historian, a teacher or
 a researcher. Define the type [specialisation] that represents those choices.
[*----------------------------------------------------------------------------*)



(*----------------------------------------------------------------------------*]
 Every wizard starts out as a newbie. Afterwards they become a student and in
 the end, they may get employed. Define the type [status] which determines if a
 wizard is:
  a.) a newbie,
  b.) a student (also what school of magic they study and for how long),
  c.) an employee (also their school of magic and specialisation).

 Then define a record type [wizard] which has a field for the wizards name and
 a field for their status.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # professor;;
 - : wizard = {name = "Matija"; status = Employed (Fire, Teacher)}
[*----------------------------------------------------------------------------*)



(*----------------------------------------------------------------------------*]
 We want to count how many users of a certain school of magic are currently in
 the group.
 Define a record type [magic_counter] which has an integer field for every
 school of magic.
 Then define the function [update counter magic] that returns a new counter
 with an updated field depending on the value of [magic].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # update {fire = 1; frost = 1; arcane = 1} Arcane;;
 - : magic_counter = {fire = 1; frost = 1; arcane = 2}
[*----------------------------------------------------------------------------*)



(*----------------------------------------------------------------------------*]
 The function [count_magic] accepts a list of wizards and counts the users of
 different schools of magic.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # count_magic [professor; professor; professor];;
 - : magic_counter = {fire = 3; frost = 0; arcane = 0}
[*----------------------------------------------------------------------------*)

let rec count_magic = ()

(*----------------------------------------------------------------------------*]
 We wish to find a possible candidate for a job offer. A student can become a
 historian after studying for at least 3 years, a researcher after 4 years and
 a teacher after 5 years.
 The function [find_candidate magic specialisation wizard_list] searches
 through the list of wizards and returns the name of a suitable candidate for
 the [specialisation] if they are studying [magic]. If there is no candidate,
 it should return [None].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let jaina = {name = "Jaina"; status = Student (Frost, 4)};;
 # find_candidate Frost Researcher [professor; jaina];;
 - : string option = Some "Jaina"
[*----------------------------------------------------------------------------*)

let rec find_candidate = ()
