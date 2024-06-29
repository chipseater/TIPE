open Mapgen

let isNone = function Chunk (_, _) -> false | None -> true
let biome_to_string = function Forest -> "F" | Desert -> "D" | Plains -> "P"

let building_to_string = function
  | Some House -> "H"
  | Some Quarry -> "Q"
  | Some Sawmill -> "S"
  | Some Farm -> "F"
  | None -> "N"

(* type building = House | Quarry | Sawmill | Farm  *)
let print_biome biome = biome |> biome_to_string |> print_string

let get_chunk_biome = function
  | Chunk (_, biome) -> biome
  | None -> raise (Invalid_argument "Manipulating an empty chunk")

let print_chunk_biome chunk =
  assert (not (isNone chunk));
  chunk |> get_chunk_biome |> print_biome

let get_tile_z = function Tile (_, z) -> z
let get_tile_building = function Tile (building, _) -> building

let get_chunk_tiles chunk =
  match chunk with
  | Chunk (tiles, _) -> tiles
  | None -> raise (Invalid_argument "Manipulating an empty chunk")

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
(* 
let mutate_building_in_chunk chunk building i j =
  let tile_z = get_chunk_z (get_chunk_tiles chunk).(i).(j) in
  (get_chunk_tiles chunk).(i).(j) <- Chunk (tile_z, building) *)

  