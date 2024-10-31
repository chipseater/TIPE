open Mapgen

(* Fonctions utilitaires pour gérer les objets de la carte *)
let isNone = function Chunk (_, _) -> false
let biome_to_string = function Forest -> "F" | Desert -> "D" | Plains -> "P"

let building_to_string = function
  | House -> "H"
  | Quarry -> "Q"
  | Sawmill -> "S"
  | Farm -> "F"

let option_building_to_string = function
  | Some building -> building_to_string building
  | None -> "N"

let print_building building = building |> building_to_string |> print_string
let print_biome biome = biome |> biome_to_string |> print_string
let get_chunk_biome = function Chunk (_, biome) -> biome

let print_chunk_biome chunk =
  assert (not (isNone chunk));
  chunk |> get_chunk_biome |> print_biome

let get_tile_z = function Tile (_, z) -> z
let get_tile_building = function Tile (building, _) -> building
let get_chunk_tiles chunk = match chunk with Chunk (tiles, _) -> tiles

let get_chunk_buildings chunk =
  let chunk_tiles = get_chunk_tiles chunk in
  let rec make_building_list i =
    let x = i mod chunk_width in
    let y = i / chunk_width in
    if i >= 0 then
      match get_tile_building chunk_tiles.(x).(y) with
      | None -> make_building_list (i - 1)
      | Some building -> building :: make_building_list (i - 1)
    else []
  in
  make_building_list ((chunk_width * chunk_width) - 1)

let get_chunk_z chunk =
  assert (not (isNone chunk));
  let chunk_z = Array.make_matrix chunk_width chunk_width 0 in
  for i = 0 to chunk_width - 1 do
    for j = 0 to chunk_width - 1 do
      let tile = (get_chunk_tiles chunk).(i).(j) in
      chunk_z.(i).(j) <- get_tile_z tile
    done
  done;
  chunk_z

let mutate_building_in_chunk map chunk building i j =
  let tile_z = get_tile_z (get_chunk_tiles chunk).(i).(j) in
  let chunk_biome = get_chunk_biome chunk in
  let new_tiles = get_chunk_tiles chunk in
  new_tiles.(i).(j) <- Tile (building, tile_z);
  map.(i).(j) <- Chunk (new_tiles, chunk_biome)

let reset_chunk chunk =
  for i = 0 to chunk_width - 1 do
    for j = 0 to chunk_width - 1 do
      let tile_z = get_tile_z (get_chunk_tiles chunk).(i).(j) in
      (get_chunk_tiles chunk).(i).(j) <- Tile (None, tile_z)
    done
  done

let reset_map map =
  for i = 0 to Array.length map - 1 do
    for j = 0 to Array.length map.(0) - 1 do
      reset_chunk map.(i).(j)
    done
  done
