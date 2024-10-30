open Village
open Mapgen

let get_popuplation village =
  let stock, _ = village.logistics in
  search stock People

let calcul_score (village : village) (map : map) : int =
  (* Évite le warning de variable non utilisée *)
  let _ = map in
  get_popuplation village
