open Game
open Dumpmap
open Newgen
open Mutation
(* open Dumpmap *)

(* let _, _ = new_game 800 8 *)
let trees = gen_trees 8

let mutated_trees = mutate trees 0.5

