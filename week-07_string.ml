(* week-07_string.ml *)
(* Introduction to Computer Science (YSC1212), Sem2, 2022-2023 *)
(* Olivier Danvy <danvy@yale-nus.edu.sg> *)
(* Version of Tuesday March 14 2023 *)

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
  let () = Printf.printf "processing '%c'... \n" c in
  c;;

(*
# String.map a_char "abc";;
processing 'a'... 
processing 'b'... 
processing 'c'... 
- : string = "abc"
#
*)

(* Question 03b *)
let random_char () =
  char_of_int (Random.int 32 + int_of_char ' ');;

let char_next c =
  if c  = '\255'
  then '\000'
  else char_of_int (succ (int_of_char c));;

(* Unit Test for String Map *)
let test_string_map candidate =
  let b0 = (candidate (fun a -> a) "abc" = "abc")
  and b1 = (candidate (fun b -> b) "" = "")
  and b2 = (candidate (fun c -> 'a') "alltoa" = "aaaaaa")
  and b3 = (candidate (fun c -> char_next c) "abc" = "bcd")
  and br = (let n = Random.int 1000
            in candidate (fun c -> 'b')
                 (String.make n (random_char()))
               = (String.make n 'b'))
  in b0 && b1 && b2 && b3 && br;;
 
let string_of_char c =
  String.make 1 c;;

let string_map_up f s =
  let rec visit i =
    if i = 0
    then ""
    else let i' = pred i
         in let ih = visit i'
            in let s' = string_of_char(f(s.[i']))
               in ih ^ s'
in visit (String.length s);;

let () = assert (test_string_map string_map_up = true);;

(*
# string_map_up a_char "abc";;
processing 'a'... 
processing 'b'... 
processing 'c'... 
- : string = "abc"
#
*)

(* Question 03c *)
let string_map_down f s =
  let n = String.length s
in let last_index = n - 1
  in let rec visit i =
       if i = 0
       then ""
       else let i' = pred i
            in let ih = visit i'
               in let s' = string_of_char(f(s.[last_index - i']))
                  in s' ^ ih
     in visit n;;

let () = assert (test_string_map string_map_down = true);;

(*
# string_map_down a_char "abc";;
processing 'c'... 
processing 'b'... 
processing 'a'... 
- : string = "abc"
#
*)
 
(* ********** *)

(* Question 04a *)

(*
# String.mapi (fun i c -> a_char(char_of_int (i + int_of_char '0'))) "abc";;
processing '0'... 
processing '1'... 
processing '2'... 
- : string = "012"
*)
                                                         
(* Question 04b *)
let test_string_mapi candidate =
  let b0 = (candidate (fun i c -> char_of_int(i + int_of_char '0')) "a" = "0")
  and b1 = (candidate (fun i c -> char_of_int(i + int_of_char '0')) "01" = "01")
  and b2 = (candidate (fun i c -> char_of_int(i + int_of_char '0')) "abc" = "012")
  and br = (let a = string_of_char(random_char())
            and b = string_of_char(random_char())
            and c = string_of_char(random_char())
            and d = string_of_char(random_char())
            in candidate (fun i c -> char_of_int(i + int_of_char '0')) (a^b^c^d) = "0123")
  in b0 && b1 && b2 && br;;

let string_mapi_up f s =
  let rec visit i =
    if i = 0
    then ""
    else let i' = pred i
         in let ih = visit i'
            in ih ^ string_of_char(f i' s.[i'])
  in visit (String.length s);;

let () = assert (test_string_mapi string_mapi_up = true);;

(*
# string_mapi_up (fun i c -> a_char(char_of_int(i + int_of_char '0'))) "abc";;
processing '0'... 
processing '1'... 
processing '2'... 
- : string = "012"
# 
 *)
                                                         
(* Question 04c *)
let string_mapi_down f s =
  let n = String.length s
  in let last_index = n - 1
     in let rec visit i =
          if i = 0
          then ""
          else let i' = pred i
               in let s' = string_of_char(f i' (s.[last_index - i']))
                  in let ih = visit i'
                     in ih ^ s'
        in visit n;;

let () = assert (test_string_mapi string_mapi_down = true);;

(*
string_mapi_down (fun i c -> a_char(char_of_int (i + int_of_char '0'))) "abc";;
processing '2'... 
processing '1'... 
processing '0'... 
- : string = "012"
*)

(* ********** *)

(* Question 05a: reverse string using String.mapi *)

let test_string_reverse candidate =
  let b0 = (candidate "" = "")
  and b1 = (candidate "a" = "a")
  and b2 = (candidate "ab" = "ba")
  and b3 = (candidate "abc" = "cba")
  and br = (let a = string_of_char(random_char())
            and b = string_of_char(random_char())
            and c = string_of_char(random_char())
            and d = string_of_char(random_char())
            in candidate (a^b^c^d) = (d^c^b^a))
  in b0 && b1 && b2 && b3 && br;;

let string_reverse_gen s =
  let n = String.length s
  in let last_index = n - 1
     in String.mapi
          (fun i' _ -> s.[last_index - i'])
          s;;

let () = assert (test_string_reverse string_reverse_gen = true);;

(*
string_reverse_gen "abc";;
val string_reverse_gen : string -> string = <fun>
- : string = "cba"
 *)

(* Experiment to fix definition of string_reverse_gen 
let traced_string_length s =
  let n = String.length s
  in let () = Printf.printf "String.length \"%s\" = %n\n" s n in
     n;;

let traced_string_reverse_gen s =
  String.mapi (fun i _ -> s.[traced_string_length s - i - 1]) s;;

# traced_string_reverse_gen "abcde";;
String.length "abcde" = 5
String.length "abcde" = 5
String.length "abcde" = 5
String.length "abcde" = 5
String.length "abcde" = 5
- : string = "edcba"
#
*)

(* Question 05b *)
(* need unit test for string_reverse *)

let string_reverse_rec s =
  let n = String.length s in
  let rec visit i =
    if i = n
    then ""
    else let i' = succ i
         in let ih = visit i'
            in (string_of_char(s.[n - i']) ^ ih)
  in visit 0;;

let () = assert (test_string_reverse string_reverse_rec = true);;

(*
string_reverse_rec "abc";;
- : string = "cba"
*)
 
let string_reverse_down_rec s =
  let n = String.length s in
  let rec visit i =
    if i = 0
    then ""
    else let i' = pred i
         in let ih = visit i'
            in (string_of_char(s.[i']) ^ ih)
  in visit n;;

let () = assert (test_string_reverse string_reverse_down_rec = true);;

(*
# string_reverse_down_rec "abc";;
- : string = "cba"
*)

let () = assert (let c0 = random_char ()
                 and c1 = random_char ()
                 and c2 = random_char ()
                 in string_reverse_down_rec (warmup c0 c1 c2) = warmup c2 c1 c0);;

let () = assert (let c0 = random_char ()
                 and c1 = random_char ()
                 and c2 = random_char ()
                 in string_reverse_gen (warmup c0 c1 c2) = warmup c2 c1 c0);;

let () = assert (let c0 = random_char ()
                 and c1 = random_char ()
                 and c2 = random_char ()
                 in string_reverse_rec (warmup c0 c1 c2) = warmup c2 c1 c0);;

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

(* writing string_reverse as an instance of nat_parafold_right, demostrating that string_Reverse_rec is structurally recursive *)

let string_reverse_right s =
  nat_parafold_right 
    ""
    (fun i' ih ->
      string_of_char(s.[i']) ^ ih)
    (String.length s);;

(*
# string_reverse_right "abc";;
- : string = "cba"
# 
*)

(* ********** *)

(* Question 06 *)
let unit_test_for_Question_06 string_reverse string_append =
  let b0 = (string_reverse (string_append "abc" "def") = "fedcba")
  and b1 = (string_append (string_reverse "hello") (string_reverse "world")
            = "ollehdlrow")
  and b2 = (let s1 = string_of_char(random_char())
            and s2 = string_of_char(random_char())
            in string_reverse (string_append s1 s2)
               = string_append (string_reverse s2) (string_reverse s1))
  and b3 = (let s3 = string_of_char(random_char())
            and s4 = string_of_char(random_char())
            in string_reverse (string_append s4 s3)
               = string_append (string_reverse s3) (string_reverse s4))
  in b0 && b1 && b2 && b3;;

let () = assert (unit_test_for_Question_06 string_reverse_rec string_append = true);;
let () = assert (unit_test_for_Question_06 string_reverse_gen string_append = true);;

(* ********** *)

(* Question 07 *)
let make_palindrome_rec n =
  let () = assert (n >= 0)
  in let rec visit i =
       if i = 0 && (n mod 2 = 0)
       then ""
       else if i = 1 && (n mod 2 = 1)
       then String.make 1 (random_char())
       else let i' = pred i
            in let ih = visit i'
               in let s' = String.make 1 (random_char())
                  in s' ^ ih ^ s'
     in visit ((n + 1) / 2);;

(*
# make_palindrome_rec 0;;
- : string = ""
# make_palindrome_rec 1;;
- : string = "'"
# make_palindrome_rec 2;;
- : string = "55"
# make_palindrome_rec 3;;
- : string = "(4("
# make_palindrome_rec 4;;
- : string = "&22&"
*)

(* ********** *)

(* Question 08 *)
let test_palindrome candidate =
  let b0 = (candidate "" = true)
  and b1 = (candidate "ab" = false)
  and b2 = (candidate "aba" = true)
  and b3 = (candidate "abba" = true)
  and b4 = (candidate "xba" = false)
  and b5 = (candidate "abx" = false)
  and b6 = (candidate "xbba" = false)
  and b7 = (candidate "axba" = false)
  and b8 = (candidate "abxa" = false)
  and b9 = (candidate "abbx" = false)
  and b10 = (let a = String.make 1 (random_char())
            and b = String.make 1 (random_char())
            and c = String.make 1 (random_char())
            in candidate (a^b^c^b^a) = true)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10;;

(*
let palindromep_mapi s =
  let n = String.length s
  in let last_index = n - 1
     in String.mapi(fun i _ -> s.[last_index - i]) s
        = s;;

let () = assert (test_palindrome palindromep_mapi = true);;
 *)

let palindromep_rec s =
  let n = String.length s
  in let last_index = n - 1
  in let rec visit i =
    if i = 0
    then true
    else let i' = pred i
         in if (s.[i'] = s.[last_index - i'])
            then visit i'
            else false
  in visit (n / 2);;
  
let () = assert (test_palindrome palindromep_rec);;

(*
let traced_compare_char c1 c2 =
  let () = Printf.printf "comparing '%c' and '%c'...\n" c1 c2 in
  c1 = c2;;
*)

let palindromep_gen s =
  let n = String.length s in
  let last_index = n - 1 in
  nat_parafold_right
    true
    (fun i' ih -> if(s.[i'] = s.[last_index - i']) then ih else false)
    (n / 2);;

let () = assert (test_palindrome palindromep_gen = true);;

(* ********** *)

(* Question 09 (not optional) *)
(* SMILEY :)) *)

(* ********** *)

(* Question 10 *)
let string_map_gen f s =
  String.mapi (fun i _ -> (f s.[i])) s;;

let () = assert (test_string_map string_map_gen);;

(* ********** *)

(* Question 11 *)
let string_mapi_gen f s =
  String.init
    (String.length s)
    (fun i -> f i (s.[i]));;

let () = assert (test_string_mapi string_mapi_gen);;

(* ********** *)

(* Question 12a *)
let digitp c =
  '0' <= c && c <= '9';;

let test_string_andmap candidate =
  let b0 = (candidate digitp "" = true)
  and b1 = (candidate digitp "123" = true)
  and b2 = (candidate digitp "abc" = false) 
  and b3 =(candidate digitp "123abc" = false)
  in b0 && b1 && b2 && b3;;

(* string_andmap takes a predicate and a string and applies the predicate to each character of the string; it returns the conjuntion of the result *)
let string_andmap_rec f s =
  let rec visit i =
    if i = 0
    then true
    else let i' = pred i
         in let ih = visit i'
            in f (s.[i']) && ih
  in visit (String.length s);;

let string_andmap f s =
  nat_parafold_right
    true
    (fun i' ih -> f (s.[i']) && ih)
    (String.length s);;

let () = assert (test_string_andmap string_andmap_rec = true);;
let () = assert (test_string_andmap string_andmap = true);;

let a_string s =
 (* a_string : string -> string *)
  let () = Printf.printf "processing \"%s\"...\n" s in
  s;;

let traced_string_andmap f s =
  string_andmap (fun x -> digitp (a_char x)) s;;

(*
# traced_string_andmap (fun x -> x) "abc";;
processing 'a'... 
processing 'b'... 
processing 'c'... 
- : bool = false
# traced_string_andmap (fun x -> x) "1234";;
processing '1'... 
processing '2'... 
processing '3'... 
processing '4'... 
- : bool = true
# traced_string_andmap (fun x -> x) "12cd";;
processing '1'... 
processing '2'... 
processing 'c'... 
processing 'd'... 
- : bool = false
# traced_string_andmap (fun x -> x) "a1";;
processing 'a'... 
processing '1'... 
- : bool = false
#
*)

(* Question 12b *)
let test_string_ormap candidate =
  (candidate digitp "" = false) &&
  (candidate digitp "123" = true) &&
  (candidate digitp "abc" = false) &&
  (candidate digitp "123abc" = true);;
  
let string_ormap_rec f s =
  let rec visit i =
    if i = 0
    then false
    else let i' = pred i
         in let ih = visit i'
            in f (s.[i']) || ih
  in visit (String.length s);;

let string_ormap f s =
  nat_parafold_right
    false
    (fun i' ih -> f (s.[i']) || ih)
    (String.length s);;

let () = assert (test_string_ormap string_ormap_rec = true);;
let () = assert (test_string_ormap string_ormap = true);;

let traced_string_ormap f s =
  string_ormap (fun x -> digitp (a_char x)) s;;

(*
  # traced_string_ormap (fun x -> x) "abc";;
processing 'a'... 
processing 'b'... 
processing 'c'... 
- : bool = false
# traced_string_ormap (fun x -> x) "123";;
processing '1'... 
processing '2'... 
processing '3'... 
- : bool = true
# traced_string_ormap (fun x -> x) "1b3";;
processing '1'... 
processing 'b'... 
processing '3'... 
- : bool = true
#
 *)

(* ********** *)

(* Question 13a *)

let random_char_int () =
  char_of_int(Random.int 9 + int_of_char '0')

(* unit test that has type (int -> char -> bool) -> string -> bool *)
let test_string_andmapi candidate =
  let b0 = (candidate (fun i c -> digitp c) "" = true)
  and b1 = (candidate (fun i c -> digitp c) "1" = true)
  and b2 = (candidate (fun i c -> digitp c) "12" = true)
  and b3 = (candidate (fun i c -> digitp c) "123" = true)
  and b4 = (candidate (fun i c -> digitp c) "x23" = false)
  and b5 = (candidate (fun i c -> digitp c) "1x3" = false)
  and b6 = (candidate (fun i c -> digitp c) "12x" = false)
  and b7 = (candidate
              (fun i c -> digitp (char_of_int(i + int_of_char '0'))) "1234" = true)
  and br = (let a = string_of_char(random_char_int())
            and b = string_of_char(random_char_int())
            and c = string_of_char(random_char_int())
            in candidate (fun i c -> digitp c) (a^b^c) = true)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && br;;

let string_andmapi_rec f s =
  let rec visit i =
    if i = 0
    then true
    else let i' = pred i
         in let ih = visit i'
            in f i' (s.[i']) && ih
  in visit (String.length s);;

let string_andmapi f s =
  nat_parafold_right
    true
    (fun i' ih -> (f i' s.[i']) && ih)
    (String.length s);;
                      
let () = assert (test_string_andmapi string_andmapi_rec = true);;
let () = assert (test_string_andmapi string_andmapi = true);; 

(* Question 13b *)
let test_string_ormapi candidate =
  let b0 = (candidate (fun i c -> digitp c) "" = false)
  and b1 = (candidate (fun i c -> digitp c) "abc" = false)
  and b2 = (candidate (fun i c -> digitp c) "abcdef" = false)
  and b3 = (candidate (fun i c -> digitp c) "123" = true)
  in b0 && b1 && b2 && b3;;
                                                 
let string_ormapi_rec f s =
  let rec visit i =
    if i = 0
    then false
    else let i' = pred i
         in let ih = visit i'
            in f i' (s.[i']) || ih
  in visit (String.length s);;

let string_ormapi f s =
  nat_parafold_right
Firs    false
    (fun i' ih -> (f i' s.[i']) || ih)
    (String.length s);;

let () = assert (test_string_ormapi string_ormapi_rec = true);;
let () = assert (test_string_ormapi string_ormapi = true);;


(* ********** *)

(* Question 14 *)
let test_check_string_append candidate =
  let b0 = (candidate "" "" "" = true)
  and b1 = (candidate "" "abc" "abc" = true)
  and b2 = (candidate "abc" "def" "abcdef" = true)
  and b3 = (candidate "abc" "" "abc" = true)
  and b4 = (candidate "1234" "1234" "12345678" = false)
  and b5 = (candidate "asdf" "adf" "adsfgasdg" = false)
  and br = (let s1 = String.map (fun _ -> random_char()) (String.make 3 ' ')
            and s2 = String.map (fun _ -> random_char()) (String.make 4 ' ')
            in candidate s1 s2 (s1 ^ s2) = true)
  in b0 && b1 && b2 && b3 && b4 && b5 && br;;

(*
let check_string_append s1 s2 s1_s2 =
  let n1 = String.length s1 
  and n2 = String.length s2
  in if n1 + n2 = String.length (s1_s2)
     then string_andmapi
            (fun i c ->
              if i < String.length s1
              then s1.[i] = s1_s2.[i]
              else s2.[i - n1] = s1_s2.[i])
            s1_s2
     else false;;
 *)

let check_string_append s1 s2 s1_s2 =
  let concatenated_string = s1 ^ s2 in
  if (String.length concatenated_string) = String.length s1_s2
  then string_andmapi (fun i' _ -> concatenated_string.[i'] = s1_s2.[i']) s1_s2
  else false;;

let () = assert (test_check_string_append check_string_append = true);;

(* ********** *)

(* Quesiton 15 *)
let test_check_string_map candidate =
  let b0 = (candidate (fun _ -> 'x') "" "" = true)
  and b1 = (candidate (fun _ -> 'x') "ab" "xx" = true)
  and b2 = (let n = Random.int 100
            and c = random_char() in
            let s1 = String.make n (random_char())
            and s2 = String.make n c
            in candidate  (fun _ -> c) s1 s2 = true)
  and b3 = (candidate (fun _ -> 'x') "abc" "xx" = false)
  and b4 = (candidate (fun _ -> 'x') "abc" "abc" = false)
  in b0 && b1 && b2 && b3 && b4;;

let check_string_map f s r =
  if (String.length s) = String.length r
  then string_andmapi (fun i c -> f (s.[i]) = c) r
  else false;;
  
let () = assert (test_check_string_map check_string_map);;

(* ********** *)

(* Question 16 *)
let test_check_string_mapi candidate =
  let b0 = (candidate (fun i c -> char_of_int(i + int_of_char '0')) "abc" "012" = true)
  and b1 = (candidate (fun i c -> char_of_int(i + int_of_char '0')) "123" "012" = true)
  and b2 = (candidate (fun i c -> char_of_int(i + int_of_char '0')) "bca" "bca" = false)
  and b3 = (candidate (fun i c -> char_of_int(i + int_of_char '0')) "123" "abc" = false)
  and br = (candidate (fun i c -> char_of_int(i + int_of_char '0'))
              (String.make 4 (random_char())) "abcd" = false)
  in b0 && b1 && b2 && b3 && br;;

let check_string_mapi f s r =
  if String.length s = String.length r
  then string_andmapi (fun i' c -> f i' (s.[i']) = c) r
  else false;;
  
let () = assert (test_check_string_mapi check_string_mapi);;

(* ********** *)

(* Question 17 *)

let test_check_string_reverse candidate =
  let b0 = (candidate "" "" = true)
  and b1 = (candidate "a" "a" = true)
  and b2 = (candidate "ab" "ba" = true)
  and b3 = (candidate "abc" "cba" = true)
  and b4 = (candidate "abcd" "abcd" = false)
  and b5 = (candidate "abcd" "abc" = false)
  and b6 = (let a = String.make 3 (random_char())
            and b = String.make 4 (random_char())
            in candidate a b = false)
  and br = (let c = String.make 1 (random_char())
            and d = String.make 1 (random_char())
            and e = String.make 1 (random_char())
            in candidate (c^d^e) (e^d^c) = true)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && br;;

let check_string_reverse s s_op =
  let last_index = (String.length s) - 1 in
  if (String.length s) = String.length s_op
  then string_andmapi (fun i' c -> (s.[last_index - i'] = c)) s_op
  else false;;

let () = assert (test_check_string_reverse check_string_reverse);;

(* ********** *)

(* Question 18a *)
let test_string_appendmap candidate =
  let b0 = (candidate (fun c -> String.make 2 c) "abc" = "aabbcc")
  and b1 = (candidate (fun c -> String.make 2 c) "" = "")
  and b2 = (candidate (fun c -> "xx") "abc" = "xxxxxx")
  in b0 && b1 && b2;;

let string_appendmap f s =
  nat_parafold_right
    ""
    (fun i' ih -> ih ^ (f s.[i']))
    (String.length s);;

let () = assert (test_string_appendmap string_appendmap);;

(* Question 18b *)
let test_show_string candidate =
  (candidate "" = "\"\"") &&
  (candidate "abc" = "\"abc\"") &&
  (candidate "ab\\cd" = "\"ab\\\\cd\"") &&
  (candidate "\"" = "\"\\\"\"");;

(* Rationale:
   # Printf.printf "%s\n" "\"\"";;
   ""
   - : unit = ()
   # Printf.printf "%s\n" "\"abc\"";;
   "abc"
   - : unit = ()
   # Printf.printf "%s\n" "\"ab\\\\cd\"";;
   "ab\\cd"
   - : unit = ()
   # Printf.printf "%s\n" "\"\\\"\"";;
   "\""
   - : unit = ()
   # 
*)

let show_string s =
  let check = 
    string_appendmap (fun c ->
        if c = '\\'
        then "\\\\"
        else if c = '\"'
        then "\\\""
        else String.make 1 c)
      s
  in "\"" ^ check ^ "\"";;

(* Wanted:
   # Printf.printf "%s\n" (show_string "");;
   ""
   - : unit = ()
   # Printf.printf "%s\n" (show_string "abc");;
   "abc"
   - : unit = ()
   # Printf.printf "%s\n" (show_string "ab\\cd");;
   "ab\\cd"
   - : unit = ()
   # Printf.printf "%s\n" (show_string "\"");;
   "\""
   - : unit = ()
   # 
 *)

let () = assert (test_show_string show_string);;

(* ********** *)

(* Question 19 *)
let test_string_appendmapi candidate =
  let b0 = (candidate (fun i c -> String.make 2 c) "abc" = "aabbcc")
  and b1 = (candidate (fun i c -> String.make 2 c) "" = "")
  and b2 = (candidate (fun i c -> "xx") "abc" = "xxxxxx")
  in b0 && b1 && b2;;

let string_appendmapi f s =
  nat_parafold_right
    ""
    (fun i' ih -> ih ^ (f i' s.[i']))
    (String.length s);;

let () = assert (test_string_appendmapi string_appendmapi = true);;

(* ********** *)

(* Question 20a *)

let string_map_gen f s =
  string_appendmap (fun c -> String.make 1 (f c)) s;;

let () = assert (test_string_map string_map_gen = true);;

(* Question 20b *)
let string_mapi_gen f s =
  string_appendmapi (fun i c -> String.make 1 (f i c)) s;;

let () = assert (test_string_mapi string_mapi_gen = true);;
              
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
val random_char : unit -> char = <fun>
val char_next : char -> char = <fun>
val test_string_map : ((char -> char) -> string -> string) -> bool = <fun>
val string_of_char : char -> string = <fun>
val string_map_up : (char -> char) -> string -> string = <fun>
val string_map_down : (char -> char) -> string -> string = <fun>
val test_string_mapi : ((int -> 'a -> char) -> string -> string) -> bool =
  <fun>
val string_mapi_up : (int -> char -> char) -> string -> string = <fun>
val string_mapi_down : (int -> char -> char) -> string -> string = <fun>
val test_string_reverse : (string -> string) -> bool = <fun>
val string_reverse_gen : string -> string = <fun>
val string_reverse_rec : string -> string = <fun>
val string_reverse_down_rec : string -> string = <fun>
val nat_parafold_right : 'a -> (int -> 'a -> 'a) -> int -> 'a = <fun>
val string_reverse_right : string -> string = <fun>
val unit_test_for_Question_06 :
  (string -> string) -> (string -> string -> string) -> bool = <fun>
val make_palindrome_rec : int -> string = <fun>
val test_palindrome : (string -> bool) -> bool = <fun>
val palindromep_rec : string -> bool = <fun>
val palindromep_gen : string -> bool = <fun>
val string_map_gen : (char -> char) -> string -> string = <fun>
val string_mapi_gen : (int -> char -> char) -> string -> string = <fun>
val digitp : char -> bool = <fun>
val test_string_andmap : ((char -> bool) -> string -> bool) -> bool = <fun>
val string_andmap_rec : (char -> bool) -> string -> bool = <fun>
val string_andmap : (char -> bool) -> string -> bool = <fun>
val a_string : string -> string = <fun>
val traced_string_andmap : 'a -> string -> bool = <fun>
val test_string_ormap : ((char -> bool) -> string -> bool) -> bool = <fun>
val string_ormap_rec : (char -> bool) -> string -> bool = <fun>
val string_ormap : (char -> bool) -> string -> bool = <fun>
val traced_string_ormap : 'a -> string -> bool = <fun>
val random_char_int : unit -> char = <fun>
val test_string_andmapi : ((int -> char -> bool) -> string -> bool) -> bool =
  <fun>
val string_andmapi_rec : (int -> char -> bool) -> string -> bool = <fun>
val string_andmapi : (int -> char -> bool) -> string -> bool = <fun>
val test_string_ormapi : (('a -> char -> bool) -> string -> bool) -> bool =
  <fun>
val string_ormapi_rec : (int -> char -> bool) -> string -> bool = <fun>
val string_ormapi : (int -> char -> bool) -> string -> bool = <fun>
val test_check_string_append : (string -> string -> string -> bool) -> bool =
  <fun>
val check_string_append : string -> string -> string -> bool = <fun>
val test_check_string_map :
  (('a -> char) -> string -> string -> bool) -> bool = <fun>
val check_string_map : (char -> char) -> string -> string -> bool = <fun>
val test_check_string_mapi :
  ((int -> 'a -> char) -> string -> string -> bool) -> bool = <fun>
val check_string_mapi : (int -> char -> char) -> string -> string -> bool =
  <fun>
val test_check_string_reverse : (string -> string -> bool) -> bool = <fun>
val check_string_reverse : string -> string -> bool = <fun>
val test_string_appendmap : ((char -> string) -> string -> string) -> bool =
  <fun>
val string_appendmap : (char -> string) -> string -> string = <fun>
val test_show_string : (string -> string) -> bool = <fun>
val show_string : string -> string = <fun>
val test_string_appendmapi :
  (('a -> char -> string) -> string -> string) -> bool = <fun>
val string_appendmapi : (int -> char -> string) -> string -> string = <fun>
val string_map_gen : (char -> char) -> string -> string = <fun>
val string_mapi_gen : (int -> char -> char) -> string -> string = <fun>
val end_of_file : string = "week-07_string.ml"
*)
