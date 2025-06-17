open Mapgen
open Type
open Variable

(* Fonctions utilitaires pour gérer les objets de la carte *)
let isNone = function Troncon (_, _) -> false

let biome_a_string = function
  | Forest -> "Forest"
  | Desert -> "Desert"
  | Plains -> "Plains"

let batiment_a_string = function
  | Maison -> "Maison"
  | Carriere -> "Carriere"
  | Scierie -> "Scierie"
  | Ferme -> "Ferme"
  | Puit -> "Puit"
  | Auberge -> "Auberge"
  | Statue -> "Statue"

let option_batiment_a_string = function
  | Some batiment -> batiment_a_string batiment
  | None -> "N"

let print_batiment batiment = batiment |> batiment_a_string |> print_string
let print_biome biome = biome |> biome_a_string |> print_string
let recup_troncon_biome = function Troncon (_, biome) -> biome

let print_troncon_biome troncon =
  assert (not (isNone troncon));
  troncon |> recup_troncon_biome |> print_biome

let recup_tuile_z = function Tuile (_, z) -> z
let recup_tuile_batiment = function Tuile (batiment, _) -> batiment

let recup_troncon_tuiles troncon =
  match troncon with Troncon (tuiles, _) -> tuiles

let recup_troncon_batiments troncon =
  let troncon_tuiles = recup_troncon_tuiles troncon in
  let rec make_batiment_list i =
    let x = i mod taille_troncon in
    let y = i / taille_troncon in
    if i >= 0 then
      match recup_tuile_batiment troncon_tuiles.(x).(y) with
      | None -> make_batiment_list (i - 1)
      | Some batiment -> batiment :: make_batiment_list (i - 1)
    else []
  in
  make_batiment_list ((taille_troncon * taille_troncon) - 1)

let recup_troncon_z troncon =
  assert (not (isNone troncon));
  let troncon_z = Array.make_matrix taille_troncon taille_troncon 0 in
  for i = 0 to taille_troncon - 1 do
    for j = 0 to taille_troncon - 1 do
      let tuile = (recup_troncon_tuiles troncon).(i).(j) in
      troncon_z.(i).(j) <- recup_tuile_z tuile
    done
  done;
  troncon_z

let modifie_batiment_dans_troncon carte troncon batiment i j x y =
  let tuile_z = recup_tuile_z (recup_troncon_tuiles troncon).(i).(j) in
  let troncon_biome = recup_troncon_biome troncon in
  let nouvelles_tuiles = recup_troncon_tuiles troncon in
  nouvelles_tuiles.(i).(j) <- Tuile (batiment, tuile_z);
  carte.(x).(y) <- Troncon (nouvelles_tuiles, troncon_biome)

let reset_troncon troncon =
  for i = 0 to taille_troncon - 1 do
    for j = 0 to taille_troncon - 1 do
      let tuile_z = recup_tuile_z (recup_troncon_tuiles troncon).(i).(j) in
      (recup_troncon_tuiles troncon).(i).(j) <- Tuile (None, tuile_z)
    done
  done;
  troncon

let reset_carte carte =
  for i = 0 to Array.length carte - 1 do
    for j = 0 to Array.length carte.(0) - 1 do
      carte.(i).(j) <- reset_troncon carte.(i).(j)
    done
  done

let copier_troncon troncon =
  Troncon
    ( Array.(map copy) (recup_troncon_tuiles troncon),
      recup_troncon_biome troncon )

let copier_ligne_carte carte i = Array.map copier_troncon carte.(i)

let copier_carte carte =
  Array.mapi (fun i _ -> copier_ligne_carte carte i) carte
