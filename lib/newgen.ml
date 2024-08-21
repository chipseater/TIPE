open Village
open Mapgen
open Random

let rnd_ressource () =
  match Random.int 5 with
  | 0 -> Food
  | 1 -> People
  | 2 -> Stone
  | 3 -> Wood
  | _ -> Bed

let rnd_ing () = match Random.int 2 with 1 -> More | _ -> Less

let gen_cond () =
  let ress1, ress2, ing, threshold =
    (rnd_ressource (), rnd_ressource (), rnd_ing (), Random.int 10)
  in
  match Random.int 4 with
  | 1 -> Ingpercent (ress1, ress2, ing, threshold)
  | 2 -> Ingflat (ress1, ress2, ing, threshold)
  | 3 -> Equalpercent (ress1, ress2, threshold)
  | _ -> Equalflat (ress1, ress2, threshold)

let gen_placement () = match Random.int 2 with 1 -> InCity | _ -> OutCity

let gen_prio () =
  let biome_nb = Random.int 3 in
  match Random.int 2 with 1 -> Pref (int_to_biome biome_nb) | _ -> Random

let gen_building () =
  match Random.int 4 with 1 -> Quarry | 2 -> Sawmill | 3 -> Farm | _ -> House

let gen_action () = (gen_placement (), gen_building (), gen_prio ())

let gen_tree () =
  let rec tree_generator () =
    if Random.int 3 = 0 then
      Node (gen_cond (), tree_generator (), tree_generator (), gen_action ())
    else Vide
  in
  Node (gen_cond (), tree_generator (), tree_generator (), gen_action ())

let random_pos min max =
  let x_min, y_min = min in
  let x_max, y_max = max in
  (int_in_range ~min:x_min ~max:x_max, int_in_range ~min:y_min ~max:y_max)

(* Génère k positions racines de villages parmi une grille en nxn chunks
   divisée en secteurs de taille n / k *)
let gen_village_roots n k =
  let () = self_init () in
  let quadrants_per_side = float_of_int k |> sqrt |> ceil |> int_of_float in
  let quadrant_width = n / quadrants_per_side in
  let roots = Array.make k (0, 0) in
  (* Ajoute à roots une coordonée de racine aléatoire
     pour chaque secteur de map *)
  for i = 0 to k - 1 do
    (* x, y sont les coordonées du coin haut-gauche
       du quadrant en cours *)
    let x, y =
      (i * quadrant_width mod n, i * quadrant_width * quadrant_width / n)
    in
    roots.(i) <- random_pos (x, y) (x + quadrant_width, y + quadrant_width)
  done;
  roots

(* Returns an array with randomly positioned villages *)
let new_villages map_width nb_villages =
  (* The default village stocks and production *)
  let stock = [ (Bed, 0); (Food, 0); (People, 0); (Stone, 0); (Wood, 0) ] in
  let prod = [ (Bed, 0); (Food, 0); (People, 0); (Stone, 0); (Wood, 0) ] in
  let roots = gen_village_roots (map_width / chunk_width) nb_villages in
  (* A token village to avoid type issues *)
  let empty_village = (0, Vide, ([], []), (0, 0), []) in
  let village_array = Array.make nb_villages empty_village in
  for i = 0 to nb_villages - 1 do
    (* A village containing only one chunk *)
    village_array.(i) <-
      (i, gen_tree (), (stock, prod), roots.(i), [ roots.(i) ])
  done;
  village_array
