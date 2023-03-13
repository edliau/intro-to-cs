(* mini-project_about-the-underlying-determinism-of-ocaml.ml *)
(* Introduction to Computer Science (YSC1212), Sem1, 2022-2023 *)
(* Olivier Danvy <danvy@yale-nus.edu.sg> *)
(* Version of Fri 30 Sep 2022, with a_function' *)
(* was: *)
(* Version of Sun 17 Sep 2022 *)

(* ********** *)

let an_int n =
 (* an_int : int -> int *)
  let () = Printf.printf "processing %d...\n" n in
  n;;

let a_bool b =
 (* a_bool : bool -> bool *)
  let () = Printf.printf "processing %B...\n" b in
  b;;

let a_char c =
 (* a_char : char -> char *)
  let () = Printf.printf "processing '%c'...\n" c in
  c;;

let a_string s =
 (* a_string : string -> string *)
  let () = Printf.printf "processing \"%s\"...\n" s in
  s;;

let a_unit () =
 (* a_unit : unit -> unit *)
  let () = Printf.printf "processing the unit value...\n" in
  ();;

let a_function f =
 (* a_function : ('a -> 'b) -> 'a -> 'b *)
  let () = Printf.printf "processing a function...\n" in
  fun x -> f x;;

let a_function' name f =
 (* a_function' : string -> ('a -> 'b) -> 'a -> 'b *)
  let () = Printf.printf "processing the %s function...\n" name in
  fun x -> f x;;

(* Some outputs from Question 01 *)

(*
# an_int 1 + an_int 100;;
processing 100...
processing 1...
- : int = 101 

# an_int (an_int 1 / an_int 0) + an_int 10;;
processing 10...
processing 0...
processing 1...
Exception: Division_by_zero

# an_int (an_int (an_int 1 / an_int 0) + an_int 10);;
processing 10...
processing 0...
processing 1...
Exception: Division_by_zero.
*)

(* Solution to Question 02 *)

let f x y = an_int (an_int 3 + an_int x) + an_int y;;

(*
# a_function f (an_int 1) (an_int 2);;
processing 2...
processing 1...
processing a function...
processing 2...
processing 1...
processing 3...
processing 4...
- : int = 6
*)

(* Solution to Question 04 *)

(*
# a_function (fun x1 -> fun x2 -> an_int x1 + an_int x2) (an_int 1) (an_int 2);;
processing 2...
processing 1...
processing a function...
processing 2...
processing 1...
- : int = 3

# a_function (fun (x1,x2) -> an_int x1 + an_int x2) ((an_int 1),(an_int 2));;
processing 2...
processing 1...
processing a function...
processing 2...
processing 1...
- : int = 3
 *)

(* Solution to Question 06 *)

(*
#  let a = an_int 1 and b = an_int 2 and c = an_int 3;;
processing 1...
processing 2...
processing 3...
val a : int = 1
val b : int = 2
val c : int = 3
 *)

(* Solution to Question 08 *)

(*
# a_bool true && a_bool true;;
processing true...
processing true...
- : bool = true

# a_bool true && a_bool false;;
processing true...
processing false...
- : bool = false

# a_bool false && a_bool true;;
processing false...
- : bool = false

# a_bool false && a_bool false;;
processing false...
- : bool = false
*)

(* ********** *)

let end_of_file = "mini-project_about-the-underlying-determinism-of-ocaml.ml";;
