open Domainslib.Task
open Type
open Variable

(* Sets the balance between the diffrent biomes,
   here 8 plains for 1 desert and 1 tundra *)
let int_a_biome b =
  assert (b >= 0 && b < 10);
  if b < 4 then Plains else if b < 8 then Forest else Desert

(* Checks if the point is not outside of a nxn carte *)
let is_valid n i j = not (i < 0 || i >= n || j < 0 || j >= n)

(* Generates a (n / grid_taille)^2 grid with
   a random noramlized vector at each node *)
let gen_rand_grad n grid_taille =
  let () = Random.self_init () in
  let grad_grid =
    Array.make_matrix (n / grid_taille) (n / grid_taille) (0., 0.)
  in
  for i = 0 to (n / grid_taille) - 1 do
    for j = 0 to (n / grid_taille) - 1 do
      let rand_angle = float_of_int (Random.int 720) *. Float.pi /. 360. in
      grad_grid.(i).(j) <- (cos rand_angle, sin rand_angle)
    done
  done;
  grad_grid

let smoothstep x =
  (-20. *. (x ** 7.))
  +. (70. *. (x ** 6.))
  -. (84. *. (x ** 5.))
  +. (35. *. (x ** 4.))

(* Gives a smooth appearance to the noise *)
let interpolate a b x =
  if x < 0. then 0. else if x > 1. then 1. else ((b -. a) *. smoothstep x) +. a

(* Returns the fract part of x / n, here it is used to compute
   the relative coordinates in a n-sized grid cell *)
let local_coord x n =
  let frac x = x -. Float.floor x in
  frac (float_of_int x /. float_of_int n)

let perlin grad_grid grid_taille i j =
  (* Les coordonnées locales des cellules de grille *)
  let li, lj = (local_coord i grid_taille, local_coord j grid_taille) in
  (* Les coordonées du coin en bas à gauche de la cellule de grille *)
  let tl_i_coin, tl_j_coin = (i / grid_taille, j / grid_taille) in
  (* Récupère chaque vecteur de gradien pour chaque cellule de grille *)
  let tl_grad_i, tl_grad_j = grad_grid.(tl_i_coin).(tl_j_coin) in
  let tr_grad_i, tr_grad_j = grad_grid.(tl_i_coin).(tl_j_coin + 1) in
  let bl_grad_i, bl_grad_j = grad_grid.(tl_i_coin + 1).(tl_j_coin) in
  let br_grad_i, br_grad_j = grad_grid.(tl_i_coin + 1).(tl_j_coin + 1) in
  (* Calcule le produit scalaire entre les coordonnées locales
     et le vecteur gradien de chaque coin *)
  let tl_dot_prod = (li *. tl_grad_j) +. (lj *. tl_grad_i) in
  let tr_dot_prod = (li *. tr_grad_j) +. ((lj -. 1.) *. tr_grad_i) in
  let bl_dot_prod = ((li -. 1.) *. bl_grad_j) +. (lj *. bl_grad_i) in
  let br_dot_prod = ((li -. 1.) *. br_grad_j) +. ((lj -. 1.) *. br_grad_i) in
  (* Interpolates the dot products from left to right
     then from bottom to top *)
  let top_interpolation = interpolate tl_dot_prod tr_dot_prod lj in
  let bottom_interpolation = interpolate bl_dot_prod br_dot_prod lj in
  interpolate top_interpolation bottom_interpolation li

(* Adds a layer of perlin weighted by factor to a nxn matrix carte *)
let perlin_layer (carte : float array array) n grid_taille factor =
  (* Generates a gradient grid with enough padding to work with *)
  let grad_grid = gen_rand_grad (n + (2 * grid_taille)) grid_taille in
  let perlin_pool = setup_pool ~name:"perlin_pool" ~num_domains:5 () in
  let make_cell i j =
    let raw_z = perlin grad_grid grid_taille i j in
    assert (raw_z >= -0.71 && raw_z <= 0.71);
    let z = carte.(i).(j) +. (((0.5 *. raw_z) +. 0.5) /. factor) in
    assert (z >= 0. && z <= 1.);
    carte.(i).(j) <- z
  in
  (* Sequentially computes the perlin noise for the i-th row *)
  let make_row i =
    (fun () ->
      parallel_for ~start:0 ~finish:(n - 1) ~body:(make_cell i) perlin_pool)
    |> run perlin_pool
  in
  (fun () -> parallel_for ~start:0 ~finish:(n - 1) ~body:make_row perlin_pool)
  |> run perlin_pool;
  teardown_pool perlin_pool

(* Converts a float matrice taille valeur ranging from 0 to 1
   to a int matrice with valeur ranging from 0 to the factor *)
let upscale_matrix_a_int factor (matrice : float array array) =
  let n = Array.length matrice in
  let nouvel_matrice = Array.make_matrix n n 0 in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      nouvel_matrice.(i).(j) <- int_of_float (factor *. matrice.(i).(j))
    done
  done;
  nouvel_matrice

(* Superposes octaves of noises to create fractal noise with cell taille m *)
let perlin_carte n cell_taille octaves =
  let carte = Array.make_matrix n n 0. in
  (* Sets up a pool of threads to compute the layers asyncronously *)
  let layer_pool = setup_pool ~name:"layer_pool" ~num_domains:2 () in
  let make_layer i =
    perlin_layer carte n
      (cell_taille / Utils.pow 2 i)
      (Utils.pow 2 i |> float_of_int)
  in
  (fun () -> parallel_for ~start:1 ~finish:octaves ~body:make_layer layer_pool)
  |> run layer_pool;
  teardown_pool layer_pool;
  carte

let hv_a_biome h v =
  if h *. v < 0. then Plains else if h < 0. then Desert else Forest

let gen_biomes n biome_taille =
  let carte = Array.make_matrix n n Plains in
  let humidity_grad = gen_rand_grad n biome_taille in
  let verecupation_grad = gen_rand_grad n biome_taille in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      let h = perlin humidity_grad (2 * biome_taille) i j in
      let v = perlin verecupation_grad (2 * biome_taille) i j in
      carte.(i).(j) <- hv_a_biome h v
    done
  done;
  carte

(* Generates an empty troncon according to z_valeur and a biome *)
let gen_empty_troncon (z_valeur : int array array) (biome : biome) =
  let recup_empty_tuile i j = Tuile (None, z_valeur.(i).(j)) in
  let troncon =
    Array.init_matrix taille_troncon taille_troncon recup_empty_tuile
  in
  Troncon (troncon, biome)

let gen_z n z_taille octaves =
  perlin_carte n z_taille octaves |> upscale_matrix_a_int z_max

(* Extracts a nxn sousmatrice from the top-left coin *)
let sousmatrice matrice coin n =
  let x, y = coin in
  let sousmatrice = Array.make_matrix n n 0 in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      sousmatrice.(i).(j) <- matrice.(x + i).(y + j)
    done
  done;
  sousmatrice

(* Fonction de génération de la carte
   n est la taille de la carte, nb_biomes est le nombre de poles à utiliser pour générer les biomes, z_taille est la taille des cellules du bruit de perlin et octaves est le nombre d'octaves de perlin à superposer *)
let gen_carte ?(biome_taille = 20) ?(z_taille = 100) ?(octaves = 6) n =
  let nb_of_troncon = n / taille_troncon in
  let biomes = gen_biomes n biome_taille in
  let z_carte = gen_z n z_taille octaves in
  let gen_troncon i j =
    let z_valeur =
      sousmatrice z_carte
        (i * taille_troncon, j * taille_troncon)
        taille_troncon
    in
    gen_empty_troncon z_valeur biomes.(i).(j)
  in
  Array.init_matrix nb_of_troncon nb_of_troncon gen_troncon
