open Village
open Mapgen
open Mapmanage

let get_popuplation village =
  let stock, _ = village.logistics in
  search stock People

let get_village_buildings (village : village) (map : chunk array array) =
  let rec make_village_list list =
    match list with
    | [] -> []
    | (i, j) :: q ->
      (get_chunk_buildings map.(i).(j)) @ make_village_list q
  in make_village_list (village.position_list) 

let calcul_score (village : village) (map : map) : int =
  (* Évite le warning de variable non utilisée *)
  let _ = map in
  get_popuplation village
