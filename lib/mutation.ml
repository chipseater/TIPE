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

let int_of_ing = function 2 -> More | 1 -> Equal | _ -> Less

(* Change une inégalité en pourcentage par une inégalité brute et inversement *)
let switch_condition_type condition =
  match condition with
  | Ingpercent (r1, r2, ing, int) -> Ingflat (r1, r2, ing, int)
  | Ingflat (r1, r2, ing, int) -> Ingpercent (r1, r2, ing, int)

let argument_of_int = function 1 -> OutCity | _ -> InCity

let increase_r1_amount condition increment =
  match condition with
  | Ingpercent (r1, r2, ing, int) ->
      Ingpercent (r1, r2, ing, abs (int + increment))
  | Ingflat (r1, r2, ing, int) -> Ingflat (r1, r2, ing, abs (int + increment))

let increase_r2_amount condition increment =
  match condition with
  | Ingpercent (r1, r2, ing, int) -> Ingpercent (r1, r2, ing, int + increment)
  | Ingflat (r1, r2, ing, int) -> Ingflat (r1, r2, ing, int + increment)

let increase_ress_amount condition rss_number increment =
  if rss_number = 2 then increase_r2_amount condition increment
  else if rss_number = 1 then increase_r1_amount condition increment
  else raise (Invalid_argument "rss_number should be 1 or 2")

let rnd_increase_ress condition =
  let rss_number = 1 + Random.int 1 in
  let increment = int_of_float (Utils.rand_normal 0. 4.) in
  increase_ress_amount condition rss_number increment

let change_ing condition =
  let new_ing = int_of_ing (Random.int 3) in
  match condition with
  | Ingpercent (r1, r2, _, int) -> Ingpercent (r1, r2, new_ing, int)
  | Ingflat (r1, r2, _, int) -> Ingflat (r1, r2, new_ing, int)

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
  | Random -> Pref (int_to_biome (Random.int 5))
  | Pref biome -> Random

let change_preference_biome prio =
  match prio with
  | Random -> Random
  | Pref biome -> Pref (int_to_biome (Random.int 5))

let rand_change_prio prio =
  let new_prio =
    match Random.int 2 with 1 -> change_preference_biome prio | _ -> prio
  in
  match new_prio with
  | Pref biome -> change_preference_type prio
  | Random -> change_preference_type prio

let change_threshold condition =
  match condition with
  | Ingpercent (r1, r2, ing, old_threshold) ->
      Ingpercent
        ( r1,
          r2,
          ing,
          Utils.int_rand_normal old_threshold 5)
  | Ingflat (r1, r2, ing, old_threshold) ->
      Ingflat (r1, r2, ing, Utils.int_rand_normal old_threshold 5)

let change_argument_of_action action =
  let _, building, prio = action in
  (argument_of_int (Random.int 2), building, prio)

let change_building_of_action action =
  let arg, _, prio = action in
  (arg, building_of_int (Random.int 4), prio)

let change_prio_of_action action =
  let arg, building, prio = action in
  (arg, building, rand_change_prio prio)

let mutate_action action =
  match Random.int 3 with
  | 2 -> change_argument_of_action action
  | 1 -> change_building_of_action action
  | _ -> change_prio_of_action action

let mutate_condition condition_type =
  match condition_type with
  | 3 -> switch_condition_type
  | 2 -> change_ing
  | 1 -> change_rss_type
  | _ -> rnd_increase_ress

(* Mute la racine de l'arbre avec une probabilité de p0,
   puis mute ses fils avec une proba de p = p0 * exp(-d),
   avec d la profondeur du noeud *)
let mutate_tree root_tree p0 =
  let rec tree_mutator tree p =
    match tree with
    | Vide -> Vide
    | Node (cond, l_tree, r_tree, action) ->
        if Utils.rand_bool p then
          let mutation_function = mutate_condition (Random.int 4) in
          Node ( mutation_function cond,
            tree_mutator l_tree (p *. 0.8),
            tree_mutator r_tree (p *. 0.8),
            mutate_action action )
        else tree
  in
  tree_mutator root_tree p0

let mutate tree_array p0 =
  let n = Array.length tree_array in
  let mutated_trees = Array.make (5 * n) Vide in
  for i = 0 to n - 1 do
    mutated_trees.(i) <- tree_array.(i)
  done;
  for i = n to (5 * n) - 1 do
    mutated_trees.(i) <- mutate_tree (tree_array.(i mod 5)) p0
  done;
  mutated_trees
