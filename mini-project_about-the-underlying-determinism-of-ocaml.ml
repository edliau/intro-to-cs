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

# ((an_int 1 + an_int 2) + an_int 3) + an_int 4;;
processing 4...
processing 3...
processing 2...
processing 1...
- : int = 10
 *)

(*
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
#  a_function'("outer") ((a_function' ("inner") f) (an_int 1)) (an_int 2);;
processing 2...
processing 1...
processing the inner function...
processing the outer function...
processing 2...
processing 1...
processing 3...
processing 4...
- : int = 6
# 
*)

(* Solution to Question 03 *)

(*
 # (an_int 1, an_int 2);;
processing 2...
processing 1...
- : int * int = (1, 2)

# (a_bool true, a_bool false);;
processing false...
processing true...
- : bool * bool = (true, false)
 *)

(* Solution to Question 04 *)

(*
# a_function' ("outer") (a_function' ("inner") (fun x1 -> fun x2 -> an_int x1 + an_int x2))(an_int 1) (an_int 2);;
processing 2...
processing 1...
processing the inner function...
processing the outer function...
processing 2...
processing 1...
- : int = 3

# a_function' ("outer") (a_function' ("inner") (fun (x1,x2) -> an_int x1 + an_int x2)) ((an_int 1),(an_int 2));;
processing 2...
processing 1...
processing the inner function...
processing the outer function...
processing 2...
processing 1...
- : int = 3
# 
 *)

(* Solution to Question 05 *)

(*
# a_function (fun x1 -> an_int x1 + an_int 2) (an_int 1);;
processing 1...
processing a function...
processing 2...
processing 1...
- : int = 3

# let x1 = (an_int 1) in (an_int x1 + an_int 2);;
processing 1...
processing 2...
processing 1...
- : int = 3
 *)

(* Solution to Question 06 *)

(*
#  let a = an_int 1 and b = an_int 2;;
processing 1...
processing 2...
val a : int = 1
val b : int = 2
#  let a = a_bool true and b = a_bool false;;
processing true...
processing false...
val a : bool = true
val b : bool = false
#  let a = a_char 'a' and b = a_char 'b';;
processing 'a'...
processing 'b'...
val a : char = 'a'
val b : char = 'b'
#  let a = a_string "a" and b = a_string "b";;
processing "a"...
processing "b"...
val a : string = "a"
val b : string = "b"

#  let a = an_int 1 and b = an_int 2
       in a+b;;
  processing 1...
processing 2...
- : int = 3
#  let a = a_bool true and b = a_bool false
       in a, b;;
  processing true...
processing false...
- : bool * bool = (true, false)
#  let a = a_char 'a' and b = a_char 'b'
       in a, b;;
  processing 'a'...
processing 'b'...
- : char * char = ('a', 'b')
#  let a = a_string "a" and b = a_string "b"
       in a, b;;
  processing "a"...
processing "b"...
- : string * string = ("a", "b")
# 
 *)

(* Solution to Question 07 *)

(*
#  let x1 = an_int 1 and x2 = an_int 2 in (an_int x1 + an_int x2 + an_int 3);;
processing 1...
processing 2...
processing 3...
processing 2...
processing 1...
- : int = 6

# let (x1, x2) = (an_int 1, an_int 2) in (an_int x1 + an_int 2 + an_int 3);;
processing 2...
processing 1...
processing 3...
processing 2...
processing 1...
- : int = 6
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

(* Solution to Question 09 *)

(*
# 1*0;;
- : int = 0

# 5*0;;
- : int = 0

# an_int 1 * 0;;
processing 1...
- : int = 0

# an_int 2 * 0;;
processing 2...
- : int = 0
 *)

(*
# an_int 1 * 1;;
processing 1...
- : int = 1

# an_int 2 * 1;;
processing 2...
- : int = 2
 *)

(* Solution to Question 10 *)

(*
# let x1 = a_string "pe1" and x2 = a_string "pe2" in (a_string x1, a_string x2);;
processing "pe1"...
processing "pe2"...
processing "pe2"...
processing "pe1"...
- : string * string = ("pe1", "pe2")

  
# let x2 = a_string "pe2" and x1 = a_string "pe1" in (a_string x1, a_string x2);;
processing "pe2"...
processing "pe1"...
processing "pe2"...
processing "pe1"...
- : string * string = ("pe1", "pe2")
 *)

(* Solution to Question 12 *)

(*
# (fun x -> 42) 23;;
- : int = 42

# (fun x -> 42) true;;
- : int = 42

# (fun x -> 42) 'a';;
- : int = 42

# (fun x -> 42) "123";;
- : int = 42

# (fun x -> 42) [];;
- : int = 42

#  (fun x -> 42) (1/0);;
Exception: Division_by_zero.
                  
#  (fun x -> 42) (an_int 1);;
processing 1...
- : int = 42
# 
 *)

(* ********** *)

let end_of_file = "mini-project_about-the-underlying-determinism-of-ocaml.ml";;

(* ********** *)

(*
        OCaml version 4.13.1

# #use "mini-project_about-the-underlying-determinism-of-ocaml.ml";;
val an_int : int -> int = <fun>
val a_bool : bool -> bool = <fun>
val a_char : char -> char = <fun>
val a_string : string -> string = <fun>
val a_unit : unit -> unit = <fun>
val a_function : ('a -> 'b) -> 'a -> 'b = <fun>
val a_function' : string -> ('a -> 'b) -> 'a -> 'b = <fun>
val f : int -> int -> int = <fun>
val end_of_file : string =
  "mini-project_about-the-underlying-determinism-of-ocaml.ml"
# 

 *)
