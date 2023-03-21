(* week-08_ex05&23.ml *)
(* Introduction to Computer Science (YSC1212), Sem2, 2022-2023 *)
(* Olivier Danvy <danvy@yale-nus.edu.sg> *)
(* Version of Thu 2 Mar 2022 *)

(* ********** *)

(*
Name of the group on Canvas: Week-08_Group2
 *)

(* Exercise 05 *)

let test_list_stutter3_int candidate =
  let b0 = (candidate        [] =                          [])
  and b1 = (candidate       [1] =                   [1; 1; 1])
  and b2 = (candidate    [2; 1] =          [2; 2; 2; 1; 1; 1])
  and b3 = (candidate [3; 2; 1] = [3; 3; 3; 2; 2; 2; 1; 1; 1])
  (* etc. *)
  in b0 && b1 && b2 && b3;;

let list_stutter3 xs_given =
  let rec visit xs =
    match xs with
    | [] ->
       []
    | x :: xs' ->
       let ih = visit xs'
       in x :: x :: x :: ih
  in visit xs_given;;

(* Exercise 23 *)

let test_cartesian_product_4 candidate =
  let b0 = (candidate [] [] [] [] =
              [])
  and b1 = (candidate [1] [] [] [] =
              [])
  and b2 = (candidate [] [10] [] [] =
              [])
  and b3 = (candidate [] [10] [100] [] =
              [])
  and b4 = (candidate [] [] [] [200] =
              [])
  and b5 = (candidate [1] [10] [100] [] =
              [])
  and b6 = (candidate [1] [2] [10] [100] =
              [(1, 2, 10, 100)])
  and b7 = (candidate [1; 2] [10; 20] [100] [200] =
              [(1, 10, 100, 200); (1, 20, 100, 200);
               (2, 10, 100, 200); (2, 20, 100, 200)])
  and b8 = (candidate [1; 2; 3] [10; 20] [100] [200] =
              [(1, 10, 100, 200); (1, 20, 100, 200);
               (2, 10, 100, 200); (2, 20, 100, 200);
               (3, 10, 100, 200); (3, 20, 100, 200)])
  and b9 = (candidate [1; 2; 3] [10; 20] [100; 200] [300] =
              [(1, 10, 100, 300); (1, 10, 200, 300);
               (1, 20, 100, 300); (1, 20, 200, 300);
               (2, 10, 100, 300); (2, 10, 200, 300);
               (2, 20, 100, 300); (2, 20, 200, 300);
               (3, 10, 100, 300); (3, 10, 200, 300);
               (3, 20, 100, 300); (3, 20, 200, 300)])
  and b10 = (candidate [1; 2; 3] [10; 20] [100; 200] [300; 400] =
              [(1, 10, 100, 300); (1, 10, 100, 400);
               (1, 10, 200, 300); (1, 10, 200, 400);
               (1, 20, 100, 300); (1, 20, 100, 400);
               (1, 20, 200, 300); (1, 20, 200, 400);
               (2, 10, 100, 300); (2, 10, 100, 400);
               (2, 10, 200, 300); (2, 10, 200, 400);
               (2, 20, 100, 300); (2, 20, 100, 400);
               (2, 20, 200, 300); (2, 20, 200, 400);
               (3, 10, 100, 300); (3, 10, 100, 400);
               (3, 10, 200, 300); (3, 10, 200, 400);
               (3, 20, 100, 300); (3, 20, 100, 400);
               (3, 20, 200, 300); (3, 20, 200, 400)])
  (* etc. *)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 (* etc. *);;

let cartesian_product_4 vs_given ws_given xs_given ys_given =
  let rec traverse_1 vs =
    match vs with
    | [] ->
       []
    | v :: vs' ->
       let vs'_and_ws_given_and_xs_given_and_ys_given_with_quadtuples_prepended = traverse_1 vs'
       in let rec traverse_2 ws =
            match ws with
            | [] ->
               vs'_and_ws_given_and_xs_given_and_ys_given_with_quadtuples_prepended
            | w :: ws' ->
               let vs'_and_ws'_and_xs_given_and_ys_given_with_quadtuples_prepended = traverse_2 ws'
               in let rec traverse_3 xs =
                    match xs with
                    | [] ->
                       vs'_and_ws'_and_xs_given_and_ys_given_with_quadtuples_prepended
                    | x :: xs' ->
                       let vs'_and_ws'_and_xs'_and_ys_given_with_quadtuples_prepended = traverse_3 xs'
                       in let rec traverse_4 ys =
                            match ys with
                            | [] ->
                               vs'_and_ws'_and_xs'_and_ys_given_with_quadtuples_prepended
                            | y :: ys' ->
                               let vs'_and_ws'_and_xs'_and_ys'_with_quadtuples_prepended = traverse_4 ys'
                               in (v, w, x, y) :: vs'_and_ws'_and_xs'_and_ys'_with_quadtuples_prepended
                          in traverse_4 ys_given
                  in traverse_3 xs_given
          in traverse_2 ws_given
  in traverse_1 vs_given;;

let () = assert(test_cartesian_product_4 cartesian_product_4 = true);;

(***********)

let end_of_file = "week-08_ex05&23.ml";;

(*

*)

