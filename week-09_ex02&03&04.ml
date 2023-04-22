(* week-09_ex02&03&04.ml *)
(* Introduction to Computer Science (YSC1212), Sem2, 2022-2023 *)
(* Olivier Danvy <danvy@yale-nus.edu.sg> *)
(* Version of Wed 22 Mar 2023 *)

(* ********** *)

(*
Name of the group on Canvas: Week-09_Group2
 *)
 
(* Exercise 03 *)
let test_reverse_list candidate =
  let b0 = (candidate (fun x -> x) [] = [])
  and b1 = (candidate (fun x -> x) [1] = [1])
  and b2 = (candidate (fun x -> x + 1) [1; 2] = [3; 2])
  and b3 = (candidate (fun x -> x - 1) [1; 2; 3] = [2; 1; 0])
  and br = (let a = Random.int 100
            and b = Random.int 100
            and c = Random.int 100
            and d = Random.int 100
            in candidate (fun x -> x) [a; b; c; d] = [d; c; b; a])
  in b0 && b1 && b2 && b3 && br;;

let reverse_list_rec f vs_given =
     let rec visit vs =
       match vs with
       |[] ->
         []
       | v :: vs' ->
          let v = f (List.hd vs)
          and vs' = List.tl vs
          in let ih = visit vs'
             in List.append ih [v]
     in visit vs_given;;

let () = assert (test_reverse_list reverse_list_rec = true);;

(* ********** *)

(* A useful function for our unit-tests *)
let atoi n =
  let () = assert (n >= 0) in
  let rec visit n =
    match n with
    | 0 ->
       []
    | _ ->
       let n' = n - 1
       in let ih = visit n'
          in n' :: ih
  in visit n;;
 

(* Exercise 04 *)

(* A unit test for our list reversal and prepending *)
let test_list_rev_append_int candidate =
  let b0 = (candidate [] [] = [])
  and b1 = (candidate [0] [] = [0])
  and b2 = (candidate [] [0] = [0])
  and b3 = (candidate [1; 0] [] = [0; 1])
  and b4 = (candidate [] [1; 0] = [1; 0])
  and b5 = (candidate [2; 1; 0] [1] = [0; 1; 2; 1])
  and b6 = (candidate [1] [2; 1; 0] = [1; 2; 1; 0])
  and b7 = (let ns = atoi 100
            in candidate (ns) (ns) = List.append (List.rev ns) ns)
  (* etc. *)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7;;

let rev_append_list_rec v1s v2s =
  let rec visit vs =
    match vs with
    |[] ->
      []
    | v :: vs' ->
       let v = List.hd vs
       and vs' = List.tl vs
       in let ih = visit vs'
          in List.append ih [v]
  in List.append (visit v1s) v2s;;

let () = assert (test_list_rev_append_int rev_append_list_rec = true);;

(***********)

let end_of_file = "week-09_ex02&03&04.ml";;

(*
        OCaml version 4.13.1

# #use "week-09_ex02&03&04.ml";;
val test_reverse_list : ((int -> int) -> int list -> int list) -> bool =
  <fun>
val reverse_list_rec : ('a -> 'b) -> 'a list -> 'b list = <fun>
val atoi : int -> int list = <fun>
val test_list_rev_append_int : (int list -> int list -> int list) -> bool =
  <fun>
val rev_append_list_rec : 'a list -> 'a list -> 'a list = <fun>
val end_of_file : string = "week-09_ex02&03&04.ml"
# 
*)
