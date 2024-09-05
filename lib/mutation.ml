open Village
open Mapgen

(* Change une inégalité en pourcentage par une inégalité brute et inversement *)
let toggle_condition_type = function
  | Ingpercent (r1, r2, ing, int) -> Ingflat (r1, r2, ing, int)
  | Ingflat (r1, r2, ing, int) -> Ingpercent (r1, r2, ing, int)

(* Change le type de positionnement de Incity à Outcity et inversement *)
let toggle_argument = function
  | Incity -> Outcity
  | Outcity -> Incity

let increase_r1_amount condition increment =
  match condition with
  | Ingpercent (r1, r2, ing, int) -> Ingpercent (r1 + increment, r2, ing, int)
  | Ingflat (r1, r2, ing, int) -> Ingflat (r1 + increment, r2, ing, int)

let increase_r2_amount condition increment =
  match condition with
  | Ingpercent (r1, r2, ing, int) -> Ingpercent (r1, r2 + increment, ing, int)
  | Ingflat (r1, r2, ing, int) -> Ingflat (r1, r2 + increment, ing, int)

let increase_ress_amount condition rss_number increment =
  if rss_number = 2 then increase_r2_amount increment
  else if rss_number = 1 then increase_r1_amount increment
  else raise (Invalid_argument "rss_number should be 1 or 2")

let rnd_increase_ress condition =
  let rss_number = 1 + Random.int 1 in
  let raw_increment = Utils.rand_normal 0 5 in
  (* Prevents the increment from being null *)
  let increment =
    int_of_float
    @@ if raw_increment > 0. then raw_increment +. 1. else raw_increment -. 1.
  in
  increase_ress_amount condition rss_number increment
