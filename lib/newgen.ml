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

let rnd_ing () = match Random.int 3 with 2 -> More | 1 -> Less | _ -> Equal

let gen_cond () =
  let ress1, ress2, ing, threshold =
    (rnd_ressource (), rnd_ressource (), rnd_ing (), Random.int 10)
  in
  match Random.int 2 with
  | 1 -> Ingpercent (ress1, ress2, ing, threshold)
  | _ -> Ingflat (ress1, ress2, ing, threshold)

let gen_placement () = match Random.int 2 with 1 -> InCity | _ -> OutCity

let gen_prio () =
  match Random.int 2 with
  | 1 -> Pref (Random.int 3 |> int_to_biome)
  | _ -> Random

let gen_building () =
  match Random.int 4 with 1 -> Quarry | 2 -> Sawmill | 3 -> Farm | _ -> House

let gen_action () = (gen_placement (), gen_building (), gen_prio ())

let gen_tree () =
  let rec tree_generator height =
    if height > 0 then
      Node
        ( gen_cond (),
          tree_generator (height - 1),
          tree_generator (height - 1),
          gen_action () )
    else Vide
  in
  Utils.rand_normal 3. 1. |> ceil |> int_of_float |> tree_generator
  
  let gen_trees nb_of_trees =
   Array.init nb_of_trees (fun _ -> gen_tree ())

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
      ( i * quadrant_width mod (n - quadrant_width),
        quadrant_width * (quadrant_width * i / n) )
    in
    assert (x + quadrant_width <= n && y + quadrant_width <= n);
    roots.(i) <- random_pos (x, y) (x + quadrant_width, y + quadrant_width)
  done;
  roots

