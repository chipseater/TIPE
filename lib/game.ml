open Village
open Mapgen
open Newgen

(* A generation binds a map with the villages that live inside this map *)
type generation = village array * map
type game = generation list

let new_game map_width nb_villages =
  let map = gen_map map_width in
  let roots = gen_village_roots (map_width / chunk_width) nb_villages in
  let trees = gen_trees nb_villages in
  roots, trees 
