open Type
open Variable
open Village
open Mapgen
open Newgen

let ressource_of_int i = ressource_list.(i)
let batiment_of_int i = batiment_list.(i)

let int_of_condition_type = function
  | InegaliteEnPourcentage (_, _, _, _) -> 0
  | InegaliteBrute (_, _, _, _) -> 1

let int_of_inegalite_brut = function
  | 2 -> PlusBrut
  | 1 -> EquivalentBrut
  | _ -> MoinsBrut

let int_of_percent_ing = function 1 -> MorePercent | _ -> LessPercent

(* Change une inégalité en pourcentage par une inégalité brute et inversement *)
let switch_condition_type condition =
  match condition with
  | InegaliteEnPourcentage (r1, r2, _, int) ->
      InegaliteBrute (r1, r2, int_of_inegalite_brut (Random.int 3), int)
  | InegaliteBrute (r1, r2, _, int) ->
      InegaliteEnPourcentage (r1, r2, int_of_percent_ing (Random.int 2), int)

let increase_r1_amount condition increment =
  match condition with
  | InegaliteEnPourcentage (r1, r2, ing, int) ->
      InegaliteEnPourcentage (r1, r2, ing, abs (int + increment))
  | InegaliteBrute (r1, r2, ing, int) ->
      InegaliteBrute (r1, r2, ing, abs (int + increment))

let increase_r2_amount condition increment =
  match condition with
  | InegaliteEnPourcentage (r1, r2, ing, int) ->
      InegaliteEnPourcentage (r1, r2, ing, int + increment)
  | InegaliteBrute (r1, r2, ing, int) ->
      InegaliteBrute (r1, r2, ing, int + increment)

let increase_ress_amount condition rss_number increment =
  if rss_number = 2 then increase_r2_amount condition increment
  else if rss_number = 1 then increase_r1_amount condition increment
  else raise (Invalid_argument "rss_number should be 1 or 2")

let rnd_increase_ress condition =
  let rss_number = 1 + Random.int 1 in
  let increment = int_of_float (Utils.rand_normal 0. 4.) in
  increase_ress_amount condition rss_number increment

let change_ing condition =
  let nouvel_inegalite_brut = int_of_inegalite_brut (Random.int 3) in
  let nouvel_percent_ing = int_of_percent_ing (Random.int 2) in
  match condition with
  | InegaliteEnPourcentage (r1, r2, _, int) ->
      InegaliteEnPourcentage (r1, r2, nouvel_percent_ing, int)
  | InegaliteBrute (r1, r2, _, int) ->
      InegaliteBrute (r1, r2, nouvel_inegalite_brut, int)

let change_rss_type condition =
  let nouvel_ress = ressource_of_int (Random.int (nb_ress - 1)) in
  let ress_nb = Random.int 2 in
  match condition with
  | InegaliteEnPourcentage (r1, r2, ing, int) ->
      if ress_nb = 1 then InegaliteEnPourcentage (nouvel_ress, r2, ing, int)
      else InegaliteEnPourcentage (r1, nouvel_ress, ing, int)
  | InegaliteBrute (r1, r2, ing, int) ->
      if ress_nb = 1 then InegaliteBrute (nouvel_ress, r2, ing, int)
      else InegaliteBrute (r1, nouvel_ress, ing, int)

let change_threshold condition =
  match condition with
  | InegaliteEnPourcentage (r1, r2, ing, old_threshold) ->
      InegaliteEnPourcentage (r1, r2, ing, Utils.int_rand_normal old_threshold 5)
  | InegaliteBrute (r1, r2, ing, old_threshold) ->
      InegaliteBrute (r1, r2, ing, Utils.int_rand_normal old_threshold 5)

let change_batiment () = batiment_of_int (Random.int (nb_bat - 1))
let mutate_batiment () = change_batiment ()
let mutate_nodint () = Random.int (taille_troncon * taille_troncon)

let mutate_condition condition_type =
  match condition_type with
  | 3 -> switch_condition_type
  | 2 -> change_ing
  | 1 -> change_rss_type
  | _ -> rnd_increase_ress

exception Supr

let mutatedes_nodi (b1, b2, x, boo) p0 =
  if Utils.rand_bool p0 then
    match Random.int 5 with
    | 4 -> (change_batiment (), b2, x, boo)
    | 3 -> (b1, change_batiment (), x, boo)
    | 2 -> (b1, b2, mutate_nodint (), boo)
    | 1 -> (b1, b2, x, Utils.rand_bool 0.5)
    | _ -> raise Supr
  else (b1, b2, x, boo)

let mutate_nodi (b1, b2, x, boo) p0 =
  if Utils.rand_bool p0 then
    match Random.int 4 with
    | 3 -> (b1, change_batiment (), x, boo)
    | 2 -> (b1, b2, mutate_nodint (), boo)
    | 1 -> (b1, b2, x, Utils.rand_bool 0.5)
    | _ -> (change_batiment (), b2, x, boo)
  else (b1, b2, x, boo)

let mutation_list liste p0 =
  let rec parcdes lis =
    try match lis with [] -> [] | e :: q -> mutatedes_nodi e p0 :: parcdes q
    with Supr ->
      let (e :: q) = liste in
      e :: parcdes q
  in
  let rec parc lis c =
    if c = Array.length ressource_list then parcdes lis
    else
      match lis with
      | [] ->
          if Utils.rand_bool p0 then
            [
              ( change_batiment (),
                change_batiment (),
                mutate_nodint (),
                Utils.rand_bool 0.5 );
            ]
          else []
      | e :: q -> mutate_nodi e p0 :: parc q (c + 1)
  in
  parc liste 0

(* Mute la racine de l'arbre avec une probabilité de p0,
   puis mute ses fils avec une proba de p = p0 * exp(-d),
   avec d la profondeur du noeud *)
let mutate_arbre position_arbre p0 =
  let rec arbre_mutator arbre p =
    match arbre with
    | Node (cond, l_arbre, r_arbre, action) ->
        if Utils.rand_bool p then
          let mutation_function = mutate_condition (Random.int 4) in
          Node
            ( mutation_function cond,
              arbre_mutator l_arbre (p *. p1),
              arbre_mutator r_arbre (p *. p1),
              mutate_batiment () )
        else arbre
    | Vide ->
        if Utils.rand_bool p then Node (gen_cond (), Vide, Vide, gen_batiment ())
        else Vide
  in
  arbre_mutator position_arbre p0

let mutate_arbrepos position_arbrepos p0 =
  let rec arbrepos_mutator arbrepos p =
    match arbrepos with
    | Nodi (liste, child) ->
        if Utils.rand_bool p then
          Nodi (mutation_list liste p0, arbrepos_mutator child (p *. p1))
        else arbrepos
    | Nil -> if Utils.rand_bool p then Nodi (list_mod_generator 5, Nil) else Nil
  in

  arbrepos_mutator position_arbrepos p0

let mutate arbre_array p0 =
  let n = Array.length arbre_array in
  let mutated_arbres = Array.make (ratio * n) Vide in
  for i = 0 to n - 1 do
    mutated_arbres.(i) <- arbre_array.(i)
  done;
  for i = n to (ratio * n) - 1 do
    mutated_arbres.(i) <- mutate_arbre arbre_array.(i mod n) p0
  done;
  mutated_arbres

let mutatepos arbrepos_array p0 =
  let n = Array.length arbrepos_array in
  let mutated_arbrespos = Array.make (ratio * n) Nil in
  for i = 0 to n - 1 do
    mutated_arbrespos.(i) <- arbrepos_array.(i)
  done;
  for i = n to (ratio * n) - 1 do
    mutated_arbrespos.(i) <- mutate_arbrepos arbrepos_array.(i mod n) p0
  done;
  mutated_arbrespos
