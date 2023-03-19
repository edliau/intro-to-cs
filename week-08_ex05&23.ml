(* week06_ex4&12&29.ml *)
(* Introduction to Computer Science (YSC1212), Sem2, 2022-2023 *)
(* Olivier Danvy <danvy@yale-nus.edu.sg> *)
(* Version of Thu 2 Mar 2022 *)

(* ********** *)

(*
Name of the group on Canvas: Week-06_Group2
 *)

(* Exercise 4 *)

(* first implement nat_fold_right as given *)
let nat_fold_right zero_case succ_case n_given =
  (* nat_fold_right : 'a -> ('a -> 'a) -> int -> 'a *)
  let () = assert (n_given >= 0) in
  let rec visit n =
    if n = 0
    then zero_case
    else let n' = n - 1
         in let ih = visit n'
            in succ_case ih
  in visit n_given;;

(* implement a unit test for the exponentiation function *)
let test_exponentiation p =
  (* the zero case*)
  let b0 = (p 2 0 = 1)
  and b1 = (p 2 1 = 2)
  and b2 = (p 2 2 = 4)
  and b3 = (p 2 3 = 8)       
  and b4 = (p 4 4 = 256)
  (*inductive step*)
  and b5 = (let x = Random.int 10 and n = Random.int 10 
            in p x (succ n) = x * (p x n))
             (* etc. *)
  in b0 && b1 && b2 && b3 && b4 && b5;;

(* implement the exponentiation function using nat_fold_right *)
let exponentiation_gen x n =
  nat_fold_right 1 (fun ih -> x * ih) n;;

let () = assert (test_exponentiation exponentiation_gen = true);;

(* Exercise 12 *)

(* Reusing the earlier unit test function for our implementation of nat_of_digits *)
let test_nat_of_digits candidate =
  let b1 = (candidate "9" = 9)
  and b2 = (candidate "89" = 89)
  and b3 = (candidate "789" = 789)
  and b4 = (candidate "6789" = 6789)
  and br = (let n = Random.int 10000
            in candidate (string_of_int n) = n)
  in b1 && b2 && b3 && b4 && br;;

let power x n =
  nat_fold_right 1 (fun ih -> x * ih) n;;

let nat_of_digit c =
  let () = assert ('0' <= c && c <= '9') in
  int_of_char c - int_of_char '0';;

let nat_parafold_right zero_case succ_case n =
 (* nat_parafold_right : 'a -> (int -> 'a -> 'a) -> int -> 'a *)
  let () = assert (n >= 0) in
  let rec visit i =
    if i = 0
    then zero_case
    else let i' = pred i
         in let ih = visit i'
            in succ_case i' ih    (* <-- succ_case takes two arguments *)
  in visit n;;

(* our new implementation of nat_of_digits using nat_parafold_right *)
let nat_of_digits_v2 s =
  let n = String.length s
  in let last_index = pred n
     in nat_parafold_right
          0
          (fun i' ih -> nat_of_digit (String.get s i') * power 10 (last_index - i') + ih)
          n;;

(* verify that our new implentation passes our unit test *)
let () = assert (test_nat_of_digits nat_of_digits_v2 = true);;

(* sanity check that our unit test is valid *)
let nat_of_digits_v1 s =
  let n = String.length s
  in let last_index = pred n
     in let rec visit i =
          if i = 0
          then 0
          else let i' = pred i
               in let ih = visit i'
                  in nat_of_digit (String.get s i') * power 10 (last_index - i') + ih
        in visit n;;

let () = assert (test_nat_of_digits nat_of_digits_v1 = true);;

(* Exercise 29 *)

let nat_parafold_right zero_case succ_case n =
  (* nat_parafold_right : 'a -> (int -> 'a -> 'a) -> int -> 'a *)
  let () = assert (n >= 0) in
  let rec visit i =
    if i = 0
    then zero_case
    else let i' = pred i
         in let ih = visit i'
            in succ_case i' ih    (* <-- succ_case takes two arguments *)
  in visit n;; 

let power x n =
  nat_fold_right 1 (fun ih -> x * ih) n;;

let nat_of_digit c =
  let () = assert ('0' <= c && c <= '9') in
  int_of_char c - int_of_char '0';;

let test_int_fun_rec candidate_1 candidate_2 =
  let b1 = (candidate_1 0 = candidate_2 0)
  and b2 = (candidate_1 2 = candidate_2 2)
  and b3 = (candidate_1 11 = candidate_2 11)
  and b4 = (let x = Random.int 20
            in candidate_1 x = candidate_2 x)
  in b1 && b2 && b3 && b4 ;;

let test_str_fun_rec candidate_1 candidate_2 =
  let b1 = (candidate_1 "" = candidate_2 "")
  and b2 = (candidate_1 "123" = candidate_2 "123")
  and b3 = (candidate_1 "123456789" = candidate_2 "123456789")
  and b4 = (let x = Random.int 1000000000
            in candidate_1 (string_of_int x) = candidate_2 (string_of_int x))
  in b1 && b2 && b3 && b4 ;;

let pf0 n =
  nat_parafold_right 1 (fun i' ih -> i' * ih) n;;

(* some outputs for pf0 *)

pf0 0;;
pf0 5;;
pf0 30;;

let test_pf0 candidate =
      (* the base case: *)
  let b0 = (candidate 0 = 1)
      (* some intuitive cases: *)
  and b1 = (candidate 1 = 0)
  and b2 = (candidate 2 = 0)
  and b3 = (candidate 3 = 0)
  and b4 = (candidate 4 = 0)
  and b5 = (candidate 5 = 0)
      (* instance of the induction step: *)
  and b6 = (let n = Random.int 20
            in candidate (pred n) = (pred n) * candidate n)
  (* etc. *)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6;;

let () = assert(test_pf0 pf0);;

let pf0_rec n =
  let () = assert (n >= 0) in
  let rec visit i =
    if i = 0
    then 1
    else let i' = pred i
         in let ih = visit i'
            in i' * ih
  in visit n;;

let () = assert(test_int_fun_rec pf0 pf0_rec = true)
       
let pf1 n =
  nat_parafold_right 1 (fun i' ih -> (n - i') * ih) n;;

(* some outputs for pf1 *)

pf1 0;;
pf1 5;;
pf1 30;;

let test_pf1 candidate =
      (* the base case: *)
  let b0 = (candidate 0 = 1)
      (* some intuitive cases: *)
  and b1 = (candidate 1 = 1)
  and b2 = (candidate 2 = 2)
  and b3 = (candidate 3 = 6)
  and b4 = (candidate 4 = 24)
  and b5 = (candidate 5 = 120)
      (* instance of the induction step: *)
  and b6 = (let n = Random.int 20
            in candidate (succ n) = (succ n) * candidate n)
  (* etc. *)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6;;

let () = assert(test_pf1 pf1);;

let pf1_rec n =
  let () = assert (n >= 0) in
  let rec visit i =
    if i = 0
    then 1
    else let i' = pred i
         in let ih = visit i'
            in (n-i') * ih
  in visit n;;

let () = assert(test_int_fun_rec pf1 pf1_rec = true)

let pf2 s =
  nat_parafold_right
    0
    (fun i' ih ->
      nat_of_digit (String.get s i') * power 10 i' + ih)
    (String.length s);;

(* some outputs for pf2 *)
pf2 "";;
pf2 "123";;
pf2 "123456789";;

let test_pf2 candidate =
  let b1 = (candidate "" = 0)
  and b2 = (candidate "89" = 98)
  and b3 = (candidate "789" = 987)
  and b4 = (candidate "6789" = 9876)
  in b1 && b2 && b3 && b4 ;;

let () = assert(test_pf2 pf2);;

let pf2_rec s =
  let n = String.length s
  in let rec visit i =
       if i = 0
       then 0
       else let i' = pred i
            in let ih = visit i'
               in nat_of_digit (String.get s i') * power 10 i' + ih
     in visit n;;

let () = assert(test_str_fun_rec pf2 pf2_rec = true)

let pf3 s =
  let n = String.length s
  in let last_index = n - 1
     in nat_parafold_right
          0
          (fun i' ih ->
            nat_of_digit (String.get s (last_index - i')) * power 10 i' + ih)
          n;;

(* some outputs for pf3 *)
pf3 "";;
pf3 "123";;
pf3 "123456789";;

let test_pf3 candidate =
  let b1 = (candidate "" = 0)
  and b2 = (candidate "89" = 89)
  and b3 = (candidate "789" = 789)
  and b4 = (candidate "6789" = 6789)
  and br = (let n = Random.int 10000
            in candidate (string_of_int n) = n)
  in b1 && b2 && b3 && b4 && br;;

let () = assert(test_pf3 pf3);;

let pf3_rec s =
  let n = String.length s
    in let last_index = n-1
       in let rec visit i =
            if i = 0
            then 0
            else let i' = pred i
                 in let ih = visit i'
                    in nat_of_digit (String.get s i') * power 10 (last_index - i') + ih
          in visit n;;

let () = assert(test_str_fun_rec pf3 pf3_rec = true)
       
(***********)

let end_of_file = "week06_ex4&12&29.ml";;

(*
        OCaml version 4.13.1

# #use "week06_ex4&12&29.ml";;
val nat_fold_right : 'a -> ('a -> 'a) -> int -> 'a = <fun>
val test_exponentiation : (int -> int -> int) -> bool = <fun>
val exponentiation_gen : int -> int -> int = <fun>
val test_nat_of_digits : (string -> int) -> bool = <fun>
val power : int -> int -> int = <fun>
val nat_of_digit : char -> int = <fun>
val nat_parafold_right : 'a -> (int -> 'a -> 'a) -> int -> 'a = <fun>
val nat_of_digits_v2 : string -> int = <fun>
val nat_of_digits_v1 : string -> int = <fun>
val nat_parafold_right : 'a -> (int -> 'a -> 'a) -> int -> 'a = <fun>
val power : int -> int -> int = <fun>
val nat_of_digit : char -> int = <fun>
val test_int_fun_rec : (int -> 'a) -> (int -> 'a) -> bool = <fun>
val test_str_fun_rec : (string -> 'a) -> (string -> 'a) -> bool = <fun>
val pf0 : int -> int = <fun>
- : int = 1
- : int = 0
- : int = 0
val test_pf0 : (int -> int) -> bool = <fun>
val pf0_rec : int -> int = <fun>
val pf1 : int -> int = <fun>
- : int = 1
- : int = 120
- : int = 458793068007522304
val test_pf1 : (int -> int) -> bool = <fun>
val pf1_rec : int -> int = <fun>
val pf2 : string -> int = <fun>
- : int = 0
- : int = 321
- : int = 987654321
val test_pf2 : (string -> int) -> bool = <fun>
val pf2_rec : string -> int = <fun>
val pf3 : string -> int = <fun>
- : int = 0
- : int = 123
- : int = 123456789
val test_pf3 : (string -> int) -> bool = <fun>
val pf3_rec : string -> int = <fun>
val end_of_file : string = "week06_ex4&12&29.ml"
# 
*)

