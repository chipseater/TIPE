open Village
open Mapgen
open Mapmanage
open Decision
open Type

let get_popuplation village =
  let stock, _ = village.logistique in
  recherche stock Main_d_oeuvre

let get_bonheur village =
  let stock, _ = village.logistique in
  recherche stock Bonheur

let get_village_batiments (village : village) (carte : troncon array array) =
  let rec make_village_list list =
    match list with
    | [] -> []
    | (i, j) :: q -> get_troncon_batiments carte.(i).(j) @ make_village_list q
  in
  make_village_list village.position_list

let get_statue village carte = 
  let pos_list = village.position_list in 
  let rec trouve_bat bat list = match list with
    |[] -> 0
    |(a,b)::q when a = bat -> b
    |_::q -> trouve_bat bat q 
  in
  let mat = 
    let pos_cardi = pos_card pos_list in 
    let (mats,matb) = matrice_score_troncon pos_list carte pos_cardi in 
    remp_mat_bat_list matb mats pos_list carte pos_cardi; matb 
  in 
  let c = ref 0 in 
  for i=0 to Array.length mat do 
    for j=0 to Array.length mat.(i) do
      c := !c + trouve_bat Ferme (mat.(i).(j))
    done
  done; 
  !c

let taille_lianne village = 
  let t = village.treepos in 
  let rec taille tr = match tr with 
    |Nil -> 0 
    |Nodi(_,tre) -> taille tre + 1
  in
  let rec max tr = match tr with 
    |Nil -> 0 
    |Nodi(l,tre) -> let n = List.length l in let m = max tre in if m < n then n else m
  in 
  (*(taille t)*1000 +*) max t * 100

let calcul_score (village : village) (carte : carte) : int =
  (* Évite le warning de variable non utilisée *)
  (* let _ = carte in *)
  (* get_popuplation village *)
  (* get_statue village carte *)
  (* taille_lianne village *)
  get_bonheur village
