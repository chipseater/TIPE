open Village
open Mapgen
open Mapmanage
open Decision
open Type

let get_popuplation village =
  let stock, _ = village.logistique in
  recherche stock Main_d_oeuvre

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
      c := !c + trouve_bat Statue (mat.(i).(j))
    done
  done; 
  !c

let calcul_score (village : village) (carte : carte) : int =
  (* Évite le warning de variable non utilisée *)
  (* let _ = carte in *)
  (* get_popuplation village *)
  get_statue village carte
