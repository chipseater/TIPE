open Village
open Mapgen

(* A generation binds a map with the villages that live inside this map *)
type generation = village array * map
type game = generation list
