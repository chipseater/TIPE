open Mapgen

(* Fonctions utilitaires pour gérer les objets de la carte *)
let isNone = function Troncon (_, _) -> false
let biome_to_string = function Forest -> "F" | Desert -> "D" | Plains -> "P"

let batiment_to_string = function
  | Maison -> "M"
  | Carriere -> "Ca"
  | Scierie -> "S"
  | Ferme -> "F"

let option_batiment_to_string = function
  | Some batiment -> batiment_to_string batiment
  | None -> "N"

let print_batiment batiment = batiment |> batiment_to_string |> print_string
let print_biome biome = biome |> biome_to_string |> print_string
let get_troncon_biome = function Troncon (_, biome) -> biome

let print_troncon_biome troncon =
  assert (not (isNone troncon));
  troncon |> get_troncon_biome |> print_biome

let get_tuile_z = function Tuile (_, z) -> z
let get_tuile_batiment = function Tuile (batiment, _) -> batiment
let get_troncon_tuiles troncon = match troncon with Troncon (tuiles, _) -> tuiles

let get_troncon_batiments troncon =
  let troncon_tuiles = get_troncon_tuiles troncon in
  let rec make_batiment_list i =
    let x = i mod taille_troncon in
    let y = i / taille_troncon in
    if i >= 0 then
      match get_tuile_batiment troncon_tuiles.(x).(y) with
      | None -> make_batiment_list (i - 1)
      | Some batiment -> batiment :: make_batiment_list (i - 1)
    else []
  in
  make_batiment_list ((taille_troncon * taille_troncon) - 1)

let get_troncon_z troncon =
  assert (not (isNone troncon));
  let troncon_z = Array.make_matrix taille_troncon taille_troncon 0 in
  for i = 0 to taille_troncon - 1 do
    for j = 0 to taille_troncon - 1 do
      let tuile = (get_troncon_tuiles troncon).(i).(j) in
      troncon_z.(i).(j) <- get_tuile_z tuile
    done
  done;
  troncon_z

let modifie_batiment_dans_troncon carte troncon batiment i j =
  let tuile_z = get_tuile_z (get_troncon_tuiles troncon).(i).(j) in
  let troncon_biome = get_troncon_biome troncon in
  let nouvel_tuiles = get_troncon_tuiles troncon in
  nouvel_tuiles.(i).(j) <- Tuile (batiment, tuile_z);
  carte.(i).(j) <- Troncon (nouvel_tuiles, troncon_biome)

let reset_troncon troncon =
  for i = 0 to taille_troncon - 1 do
    for j = 0 to taille_troncon - 1 do
      let tuile_z = get_tuile_z (get_troncon_tuiles troncon).(i).(j) in
      (get_troncon_tuiles troncon).(i).(j) <- Tuile (None, tuile_z)
    done
  done;
  troncon

let reset_carte carte =
  for i = 0 to Array.length carte - 1 do
    for j = 0 to Array.length carte.(0) - 1 do
      carte.(i).(j) <- reset_troncon carte.(i).(j)
    done
  done
