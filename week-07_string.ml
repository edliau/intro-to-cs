(* week-07_string.ml *)
(* Introduction to Computer Science (YSC1212), Sem2, 2022-2023 *)
(* Olivier Danvy <danvy@yale-nus.edu.sg> *)
(* Version of Fri Feb 10 2023 *)

(* ********** *)
(*
  Name of the Group on Canvas: Week-07_2
*)

(* ********** *)

(* Question 01 *)
let test_string_append candidate =
  let b0 = (candidate "a" "b" = "ab")
  and b1 = (candidate "ab" "cd" = "abcd")
  and b2 = (candidate "1" "23" = "123")
  and b3 = (candidate "str" "ing" = "string")
  and b4 = (candidate "" "nonempty" = "nonempty")
  and b5 = (candidate "nonempty" "" = "nonempty")
  and b6 = (candidate "" "" = "")
  in b0 && b1 && b2 && b3 && b4 && b5 && b6;;

let string_append s1 s2 =
  let n1 = String.length s1 in
  let n2 = String.length s2
  in String.init
       (n1 + n2)
       (fun i ->
         if i < n1
         then s1.[i]
         else s2.[i - n1]);;

let () = assert (test_string_append string_append = true);;

(* Food for thought: APPLY THIS IDEA TO ALL OTHER FUNCTIONS THAT HAS TRACING PROBLEM 
let traced_string_length s =
  let () = Printf.printf "string_length \"%s\"\n" s in
  String.length s;;

let traced_string_append s1 s2 =
  let n1 = traced_string_length s1
  in String.init
       (n1 + traced_string_length s2)
       (fun i ->
         if i < n1
         then s1.[i]
         else s2.[i - n1]);;

         traced_string_append "abc" "def";;
 *)

(* ********** *)

(* Question 02 *)
let test_warmup candidate =
  (candidate 'a''b''c' = "abc");;

let string_of_char c =
  String.make 1 c;;

let warmup c0 c1 c2 =
  string_of_char c0 ^ string_of_char c1 ^ string_of_char c2;;

let warmup_alt c0 c1 c2 =
  String.init 3
    (fun i ->
      if i = 0
      then c0
      else (if i = 1
            then c1
            else c2));;

let () = assert (test_warmup warmup = true);;
let () = assert (test_warmup warmup_alt = true);;

(* ********** *)

(* Question 03a *)
(* Tracing identity function from the underlying determininism of Ocaml *)
let a_char c =
  let () = Printf.printf "processing %c... \n" c in
  c;;

String.map a_char "abc";;

(* Question 03b *)
let random_char () =
  char_of_int (Random.int 32 + int_of_char ' ');;

let test_string_map candidate =
  let b0 = (candidate (fun a -> a) "abc" = "abc")
  and b1 = (candidate (fun b -> b) "" = "")
  and b2 = (candidate (fun c -> 'a') "alltoa" = "aaaaaa")
  and br = (let n = Random.int 1000 in
            candidate (fun c -> 'b') (String.make n (random_char())) = (String.make n 'b'))
  in b0 && b1 && b2 && br;;
                     
let string_map_up f s =
  let rec visit i =
    if i = 0
    then ""
    else let i' = pred i
         in let ih = visit i'
            in ih ^ string_of_char(f(s.[i']))
  in visit (String.length s);;

let () = assert (test_string_map string_map_up = true);;

(* Question 3c *)
let string_map_down f s =
  let rec visit i =
    if i = String.length s
    then ""
    else let i' = succ i
         in let ih = visit i'
            in string_of_char(f(s.[i' - 1])) ^ ih
  in visit 0;;

let () = assert (test_string_map string_map_down = true);;

(* ********** *)

(* Question 04a *)
(* String.mapi (fun i _ -> char_of_int (i + int_of_char '0')) "abc";; *)
let string_mapi_trace f s=
  String.mapi (fun i _ -> a_char (char_of_int(i + int_of_char '0'))) "abc";;
                                                         
(* Question 04b *)
let string_mapi_up f s =
  let rec visit i =
    if i = 0
    then ""
    else let i' = pred i
         in let ih = visit i'
            in ih ^ string_of_char(f i s.[i'])
  in visit (String.length s);;

string_mapi_up (fun i _ -> (char_of_int(i + int_of_char '0' - 1))) "abc";;
                                                         
(* Question 04c *)
let string_mapi_down f s =
  let rec visit i =
    if i = String.length s
    then ""
    else let i' = succ i
         in let ih = visit i'
            in string_of_char(f i s.[i' - 1]) ^ ih
  in visit 0;;

string_mapi_down (fun i _ -> (char_of_int(i + int_of_char '0' - 1))) "abc";;

(* ********** *)

(* Question 05a: reverse string using String.mapi *)
let string_reverse_gen s =
  String.mapi (fun i _ -> s.[String.length s - i - 1]) s;;
                                                                                        
string_reverse_gen "abc";;

(* Question 05b: reverse string using recursion over non-negative integer *)
let string_reverse_rec s =
  let n1 = String.length s in
  let () = assert (n1 >= 0) in
  let rec visit i =
    if i = n1
    then ""
    else let i' = succ i
         in let ih = visit i'
            in ih ^ string_of_char(s.[i])
in visit 0;;

string_reverse_rec "abc";;

let random_char () =
  char_of_int (Random.int 32 + int_of_char ' ');;

let c0 = random_char ()
and c1 = random_char ()
and c2 = random_char ()
    in string_reverse_gen (warmup c0 c1 c2) = warmup c2 c1 c0;;

let c0 = random_char ()
and c1 = random_char ()
and c2 = random_char ()
in string_reverse_rec (warmup c0 c1 c2) = warmup c2 c1 c0;;

(* Question 05, subsidiary *)
let nat_parafold_right zero_case succ_case n =
  let () = assert (n >= 0) in
  let rec visit i =
    if i = 0
    then zero_case
    else let i' = pred i
         in let ih = visit i'
            in succ_case i' ih
  in visit n;;
  
let string_reverse_parafold s =
  nat_parafold_right "" (fun i ih ->  string_of_char(a_char(s.[i])) ^ ih) (String.length s);;

string_reverse_parafold "abc";;

(* ********** *)

(* Question 06 *)
  let unit_test_for_Question_06 string_reverse string_append =
  let b0 = (string_reverse (string_append "abc" "def") = "fedcba")
  and b1 = (string_reverse (string_append "123" "456") = "654321")
  and b2 = (string_reverse (string_append "not" "palindrome") = "emordnilapton")
  and b3 = (string_append (string_reverse "hello") (string_reverse "world") = "ollehdlrow")
  and b4 = (string_append (string_reverse "abc") (string_reverse "def") = "cbafed")
  in b0 && b1 && b2 && b3 && b4;;

let () = assert (unit_test_for_Question_06 string_reverse_rec string_append = true);;
let () = assert (unit_test_for_Question_06 string_reverse_gen string_append = true);;

(* ********** *)

(* Question 07 *)
let make_palindrome n =
  let half = String.init (n/2) (fun i -> random_char()) in
  if (n mod 2) = 0 (* n is an even number *)
  then half ^ (string_reverse_gen(half))
  else half ^ (string_of_char (random_char())) ^ (string_reverse_gen(half));; (* n is an odd number; remember ocaml rounds the number down, for example, 3/2 = 1 *)

(* ********** *)

(* Question 08 *)
let palindrome_test candidate =
  let b0 = (candidate "aba" = true)
  and b1 = (candidate "abcdcba" = true)
  and b2 = (candidate "12345" = false)
  and b3 = (candidate "randomstring" = false)
  and b4 = (let a = String.make 1 (random_char())
            and b = String.make 1 (random_char())
            and c = String.make 1 (random_char())
            in candidate (a^b^c^b^a) = true)
  in b0 && b1 && b2 && b3 && b4;;

let palindromep_mapi s =
  let n = String.length s in
  let reverse = String.mapi (fun i _ -> s.[n - i - 1]) s
  in reverse = s;;

let () = assert (palindrome_test palindromep_mapi = true);;

let palindromep_rec s =
  let rec visit i =
    if i = 0
    then true
    else let i' = pred i
         in if (s.[i'] = s.[(String.length s) - i' - 1])
            then visit i'
            else false
  in visit ((String.length s) / 2);;
  
let () = assert (palindrome_test palindromep_rec = true);;

(* ********** *)

(* Question 09 (not optional) *)

let test_reverse_palindrome candidate =
  let b0 = (candidate "" = "")
  and b1 = (candidate "aba" = "aba")
  and b2 = (candidate "asdfdsa" = "asdfdsa")
  in b0 && b1 && b2;;

let reverse_palindrome s =
  let () = assert (palindromep_mapi s = true) in s;;

let () = assert (test_reverse_palindrome reverse_palindrome = true);;

(* ********** *)

(* Question 10 *)
let string_map_gen f s =
  String.mapi (fun i _ -> (f s.[i])) s;;

let () = assert (test_string_map string_map_gen);;

(* ********** *)


let end_of_file = "week-07_string.ml";;

(*
  OCaml version 4.14.0
  Enter #help;; for help.

# #use "week-07_string.ml";;
val test_string_append : (string -> string -> string) -> bool = <fun>
val string_append : string -> string -> string = <fun>
val test_warmup : (char -> char -> char -> string) -> bool = <fun>
val string_of_char : char -> string = <fun>
val warmup : char -> char -> char -> string = <fun>
val warmup_alt : char -> char -> char -> string = <fun>
val a_char : char -> char = <fun>
processing a... 
processing b... 
processing c... 
- : string = "abc"
val random_char : unit -> char = <fun>
val test_string_map : ((char -> char) -> string -> string) -> bool = <fun>
val string_map_up : (char -> char) -> string -> string = <fun>
val string_map_down : (char -> char) -> string -> string = <fun>
val string_mapi_trace : 'a -> 'b -> string = <fun>
val string_mapi_up : (int -> char -> char) -> string -> string = <fun>
- : string = "012"
val string_mapi_down : (int -> char -> char) -> string -> string = <fun>
- : string = "/01"
val string_reverse_gen : string -> string = <fun>
- : string = "cba"
val string_reverse_rec : string -> string = <fun>
- : string = "cba"
val random_char : unit -> char = <fun>
- : bool = true
- : bool = true
val nat_parafold_right : 'a -> (int -> 'a -> 'a) -> int -> 'a = <fun>
val string_reverse_parafold : string -> string = <fun>
processing a... 
processing b... 
processing c... 
- : string = "cba"
val unit_test_for_Question_06 :
  (string -> string) -> (string -> string -> string) -> bool = <fun>
val make_palindrome : int -> string = <fun>
val palindrome_test : (string -> bool) -> bool = <fun>
val palindromep_mapi : string -> bool = <fun>
val palindromep_rec : string -> bool = <fun>
val test_reverse_palindrome : (string -> string) -> bool = <fun>
val reverse_palindrome : string -> string = <fun>
val string_map_gen : (char -> char) -> string -> string = <fun>
val end_of_file : string = "week-07_string.ml"
# 
*)
