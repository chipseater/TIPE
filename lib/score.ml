open Village
open Mapgen
open Mapmanage

let get_popuplation village =
  let stock, _ = village.logistics in
  recherche stock Main_d_oeuvre

let get_village_batiments (village : village) (carte : troncon array array) =
  let rec make_village_list list =
    match list with
    | [] -> []
    | (i, j) :: q ->
      (get_troncon_batiments carte.(i).(j)) @ make_village_list q
  in make_village_list (village.position_list) 

let calcul_score (village : village) (carte : carte) : int =
  (* Évite le warning de variable non utilisée *)
  let _ = carte in
  get_popuplation village
