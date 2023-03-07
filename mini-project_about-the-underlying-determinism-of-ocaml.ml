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

an_int 1 + an_int 100;;

(* an_int (an_int 1 / an_int 0) + an_int 10;; *)

an_int (an_int (an_int 1 / an_int 0) + an_int 10);;

(* Solution to Question 02 *)


(* ********** *)

let end_of_file = "mini-project_about-the-underlying-determinism-of-ocaml.ml";;
