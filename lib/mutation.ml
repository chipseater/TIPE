open Village
open Mapgen

let ressource_of_int = function
  | 1 -> Food
  | 2 -> People
  | 3 -> Stone
  | 4 -> Wood
  | _ -> Bed

let building_of_int = function
  | 1 -> Quarry
  | 2 -> Sawmill
  | 3 -> Farm
  | _ -> House

let int_of_condition_type = function
  | Ingpercent (_, _, _, _) -> 0
  | Ingflat (_, _, _, _) -> 1

(* Change une inégalité en pourcentage par une inégalité brute et inversement *)
let switch_condition_type condition =
  match condition with
  | Ingpercent (r1, r2, ing, int) -> Ingflat (r1, r2, ing, int)
  | Ingflat (r1, r2, ing, int) -> Ingpercent (r1, r2, ing, int)

let argument_of_int = function 1 -> Outcity | _ -> Incity

let increase_r1_amount condition increment =
  match condition with
  | Ingpercent (r1, r2, ing, int) ->
      Ingpercent (abs (r1 + increment), r2, ing, int)
  | Ingflat (r1, r2, ing, int) -> Ingflat (abs (r1 + increment), r2, ing, int)

let increase_r2_amount condition increment =
  match condition with
  | Ingpercent (r1, r2, ing, int) ->
      Ingpercent (r1, abs (r2 + increment), ing, int)
  | Ingflat (r1, r2, ing, int) -> Ingflat (r1, abs (r2 + increment), ing, int)

let increase_ress_amount condition rss_number increment =
  if rss_number = 2 then increase_r2_amount increment
  else if rss_number = 1 then increase_r1_amount increment
  else raise (Invalid_argument "rss_number should be 1 or 2")

let rnd_increase_ress condition =
  let rss_number = 1 + Random.int 1 in
  let increment = Utils.rand_normal 0 4 in
  increase_ress_amount condition rss_number increment

let change_rss_type condition =
  let new_ress = ressource_of_int (Random.int 5) in
  let ress_nb = Random.int 2 in
  match condition with
  | Ingpercent (r1, r2, ing, int) ->
      if ress_nb = 1 then Ingpercent (new_ress, r2, ing, int)
      else Ingpercent (r1, new_ress, ing, int)
  | Ingflat (r1, r2, ing, int) ->
      if ress_nb = 1 then Ingflat (new_ress, r2, ing, int)
      else Ingflat (r1, new_ress, ing, int)

let change_preference_type prio =
  match prio with
  | Random -> Prio (int_to_biome (Random.int 5))
  | Prio biome -> Random

let change_preference_biome prio =
  match prio with
  | Random -> Random
  | Prio biome -> Prio (int_to_biome (Random.int 5))

let rand_change_prio prio =
  let new_prio =
    match Random.int 2 with
    | 1 -> change_preference_biome prio
    | 0 -> prio
  in
  match new_prio with
  | Pref biome -> change_preference_type prio
  | Random -> change_preference_type prio

let change_argument_of_action action =
  let _, building, prio = action in
  (argument_of_int (Random.int 2), building, prio)

let change_building_of_action action =
  let arg, _, prio = action in
  (arg, building_of_int (Random.int 4), prio)

let change_prio_of_action action =
  let arg, building, prio = action in
  (arg, building, rand_change_prio prio)