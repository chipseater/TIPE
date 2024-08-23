open Village
open Mapgen
open Newgen

(* A generation binds a map with the villages that live inside this map *)
type generation = village array * map
type game = generation list

let new_game map_width nb_village =
  let map = gen_map map_width in
  let villages = new_villages map_width nb_village in
  (villages, map)

