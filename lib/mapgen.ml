open Domainslib.Task

let z_max = 100.
let chunk_width = 8

type biome = Forest | Desert | Plains
type building = House | Quarry | Sawmill | Farm
type building = House | Quarry | Sawmill | Farm

(* A tile is made out of the eventual building it contains associated with its elevation *)
type tile = Tile of building option * int
(* A tile is made out of the eventual building it contains associated with its elevation *)
type tile = Tile of building option * int

(* A chunk is a 4*4 tile matrix associated with its biome *)
type chunk = Chunk of tile array array * biome

(* A n*n map is a n/4*n/4 chunk matrix *)
type map = chunk array array

(* Sets the balance between the diffrent biomes,
   here 8 plains for 1 desert and 1 tundra *)
let int_to_biome b =
  assert (b >= 0 && b < 10);
  if b < 4 then Plains else if b < 8 then Forest else Desert

(* Checks if the point is not outside of a nxn map *)
let is_valid n i j = not (i < 0 || i >= n || j < 0 || j >= n)

(* Generates a (n / grid_width)^2 grid with
   a random noramlized vector at each node *)
let gen_rand_grad n grid_width =
  if b < 4 then Plains else if b < 8 then Forest else Desert

(* Checks if the point is not outside of a nxn map *)
let is_valid n i j = not (i < 0 || i >= n || j < 0 || j >= n)

(* Generates a (n / grid_width)^2 grid with
   a random noramlized vector at each node *)
let gen_rand_grad n grid_width =
  let () = Random.self_init () in
  let grad_grid =
    Array.make_matrix (n / grid_width) (n / grid_width) (0., 0.)
  in
  for i = 0 to (n / grid_width) - 1 do
    for j = 0 to (n / grid_width) - 1 do
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

let perlin grad_grid grid_width i j =
  (* Les coordonnées locales des cellules de grille *)
  let li, lj = (local_coord i grid_width, local_coord j grid_width) in
  (* Les coordonées du coin en bas à gauche de la cellule de grille *)
  let tl_i_corner, tl_j_corner = (i / grid_width, j / grid_width) in
  (* Récupère chaque vecteur de gradien pour chaque cellule de grille *)
  let tl_grad_i, tl_grad_j = grad_grid.(tl_i_corner).(tl_j_corner) in
  let tr_grad_i, tr_grad_j = grad_grid.(tl_i_corner).(tl_j_corner + 1) in
  let bl_grad_i, bl_grad_j = grad_grid.(tl_i_corner + 1).(tl_j_corner) in
  let br_grad_i, br_grad_j = grad_grid.(tl_i_corner + 1).(tl_j_corner + 1) in
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

(* Adds a layer of perlin weighted by factor to a nxn matrix map *)
let perlin_layer (map : float array array) n grid_width factor =
  (* Generates a gradient grid with enough padding to work with *)
  let grad_grid = gen_rand_grad (n + (2 * grid_width)) grid_width in
  let perlin_pool = setup_pool ~name:"perlin_pool" ~num_domains:5 () in
  let make_cell i j =
    let raw_z = perlin grad_grid grid_width i j in
    assert (raw_z >= -0.71 && raw_z <= 0.71);
    let z = map.(i).(j) +. (((0.5 *. raw_z) +. 0.5) /. factor) in
    assert (z >= 0. && z <= 1.);
    map.(i).(j) <- z
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

(* Converts a float matrix width values ranging from 0 to 1
   to a int matrix with values ranging from 0 to the factor *)
let upscale_matrix_to_int factor (matrix : float array array) =
  let n = Array.length matrix in
  let new_matrix = Array.make_matrix n n 0 in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      new_matrix.(i).(j) <- int_of_float (factor *. matrix.(i).(j))
    done
  done;
  new_matrix

(* Superposes octaves of noises to create fractal noise with cell width m *)
let perlin_map n cell_width octaves =
  let map = Array.make_matrix n n 0. in
  (* Sets up a pool of threads to compute the layers asyncronously *)
  let layer_pool = setup_pool ~name:"layer_pool" ~num_domains:2 () in
  let make_layer i =
    perlin_layer map n
      (cell_width / Utils.pow 2 i)
      (Utils.pow 2 i |> float_of_int)
  in
  (fun () -> parallel_for ~start:1 ~finish:octaves ~body:make_layer layer_pool)
  |> run layer_pool;
  teardown_pool layer_pool;
  map

let hv_to_biome h v =
  if h *. v < 0. then Plains else if h < 0. then Desert else Forest

let gen_biomes n biome_width =
  let map = Array.make_matrix n n Plains in
  let humidity_grad = gen_rand_grad n biome_width in
  let vegetation_grad = gen_rand_grad n biome_width in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      let h = perlin humidity_grad (2 * biome_width) i j in
      let v = perlin vegetation_grad (2 * biome_width) i j in
      map.(i).(j) <- hv_to_biome h v
    done
  done;
  map

(* Generates an empty chunk according to z_values and a biome *)
let gen_empty_chunk (z_values : int array array) (biome : biome) =
  let get_empty_tile i j = Tile (None, z_values.(i).(j)) in
  let chunk = Array.init_matrix chunk_width chunk_width get_empty_tile in
  Chunk (chunk, biome)

let gen_z n z_width octaves =
  perlin_map n z_width octaves |> upscale_matrix_to_int z_max

(* Extracts a nxn submatrix from the top-left corner *)
let submatrix matrix corner n =
  let x, y = corner in
  let submatrix = Array.make_matrix n n 0 in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      submatrix.(i).(j) <- matrix.(x + i).(y + j)
    done
  done;
  submatrix

(* Fonction de génération de la carte
   n est la taille de la carte, nb_biomes est le nombre de poles à utiliser pour générer les biomes, z_width est la taille des cellules du bruit de perlin et octaves est le nombre d'octaves de perlin à superposer *)
let gen_map ?(biome_width = 20) ?(z_width = 100) ?(octaves = 6) n =
  let nb_of_chunk = n / chunk_width in
  let biomes = gen_biomes n biome_width in
  let z_map = gen_z n z_width octaves in
  let gen_chunk i j =
    let z_values =
      submatrix z_map (i * chunk_width, j * chunk_width) chunk_width
    in
    gen_empty_chunk z_values biomes.(i).(j)
  in
  Array.init_matrix nb_of_chunk nb_of_chunk gen_chunk
