let z_max = 100.
let chunk_width = 4

type biome = Forest | Desert | Plains
type building = House | Quarry | Sawmill | Farm

(* A tile is made out of the eventual building it contains associated with its elevation *)
type tile = Tile of building option * int

(* A chunk is a 4*4 tile matrix associated with its biome *)
type chunk = Chunk of tile array array * biome | None

(* A n*n map is a n/4*n/4 chunk matrix *)
type map = chunk array array

(* Contains the pole coordinates and biome *)
type pole = int * int * biome

(* Sets the balance between the diffrent biomes,
   here 8 plains for 1 desert and 1 tundra *)
let int_to_biome b =
  assert (b >= 0 && b < 10);
  if b < 4 then Plains else if b < 8 then Forest else Desert

(* Generates k random poles between 0 and n - 1 *)
let rec gen_poles n k =
  let () = Random.self_init () in
  if k = 0 then []
  else
    let pole_biome = Random.int 10 |> int_to_biome in
    (Random.int n, Random.int n, pole_biome) :: gen_poles n (k - 1)

(* Returns the euclidian distance between p2 and p1 *)
let distance p1 p2 =
  let x1, y1 = p1 in
  let x2, y2 = p2 in
  Stdlib.sqrt (float_of_int (((x2 - x1) * (x2 - x1)) + ((y2 - y1) * (y2 - y1))))

(* Returns the nearest pole from the p point *)
let rec nearest_biome p poles =
  match poles with
  | [] -> raise (Invalid_argument "Empty list")
  | [ (_, _, b) ] -> b
  (* Removes the farthest distance from the pole list *)
  | (x1, y1, b1) :: (x2, y2, b2) :: q ->
      nearest_biome p
        ((if distance (x1, y1) p < distance (x2, y2) p then (x1, y1, b1)
          else (x2, y2, b2))
        :: q)

(* Generates a nxn sized map with k biomes *)
let gen_biomes n nb_biomes =
  let map = Array.make_matrix n n Plains in
  let poles = gen_poles n nb_biomes in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      map.(i).(j) <- nearest_biome (i, j) poles
    done
  done;
  map

(* Checks if the point is not outside of a nxn map *)
let is_valid n i j = not (i < 0 || i >= n || j < 0 || j >= n)

(* Returns the average of the neighbouring square in a nxn map *)
let average_adjacent map n i j =
  let sum = ref 0. in
  let count = ref 0. in
  (* Cycles through the adjacent tiles and counts
     their number while summing their values to average them *)
  for k = 0 to 3 do
    let i_offset, j_offset = [| (-1, 0); (1, 0); (0, -1); (0, 1) |].(k) in
    let new_i, new_j = (i + i_offset, j + j_offset) in
    if is_valid n new_i new_j then (
      sum := !sum +. map.(new_i).(new_j);
      count := !count +. 1.)
  done;
  !sum /. !count

let average_map map =
  let n = Array.length map in
  let interpolated_map = Array.make_matrix n n 0. in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      interpolated_map.(i).(j) <- average_adjacent map n i j
    done
  done;
  interpolated_map

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
  (* The local coordinates in the grid cells *)
  let li, lj = (local_coord i grid_width, local_coord j grid_width) in
  (* The bottom-left coner coordinates of the grid cell *)
  let tl_i_corner, tl_j_corner = (i / grid_width, j / grid_width) in
  (* Gets each gradient vector at each corner of the grid cell *)
  let tl_grad_i, tl_grad_j = grad_grid.(tl_i_corner).(tl_j_corner) in
  let tr_grad_i, tr_grad_j = grad_grid.(tl_i_corner).(tl_j_corner + 1) in
  let bl_grad_i, bl_grad_j = grad_grid.(tl_i_corner + 1).(tl_j_corner) in
  let br_grad_i, br_grad_j = grad_grid.(tl_i_corner + 1).(tl_j_corner + 1) in
  (* Computes the dot product between the local coordinates
     and the gradient vector at each corner *)
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
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      let raw_z = perlin grad_grid grid_width i j in
      assert (raw_z >= -0.71 && raw_z <= 0.71);
      let z = map.(i).(j) +. (((0.5 *. raw_z) +. 0.25) /. factor) in
      assert (z <= 1.);
      map.(i).(j) <- z
    done
  done

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
  let factor = ref 1 in
  for _ = 1 to octaves do
    (* Weights the layer by factor = 2^i *)
    let width = cell_width / !factor in
    perlin_layer map n width (float_of_int !factor);
    factor := !factor * 2
  done;
  map

(* Generates an empty chunk according to z_values and a biome *)
let gen_empty_chunk (z_values : int array array) (biome : biome) =
  let chunk = Array.make_matrix chunk_width chunk_width (Tile (None, 0)) in
  for i = 0 to chunk_width - 1 do
    for j = 0 to chunk_width - 1 do
      let z = z_values.(i).(j) in
      chunk.(i).(j) <- Tile (None, z)
    done
  done;
  Chunk (chunk, biome)

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
let gen_map ?(nb_biomes=100) ?(z_width=100) ?(octaves=7) n =
  let nb_of_chunk = n / chunk_width in
  let map = Array.make_matrix nb_of_chunk nb_of_chunk None in
  let biomes = gen_biomes n nb_biomes in
  let z_map = perlin_map n z_width octaves |> upscale_matrix_to_int z_max in
  for i = 0 to nb_of_chunk - 1 do
    for j = 0 to nb_of_chunk - 1 do
      let z_values =
        submatrix z_map (i * chunk_width, j * chunk_width) chunk_width
      in
      let biome = biomes.(i).(j) in
      map.(i).(j) <- gen_empty_chunk z_values biome
    done
  done;
  map
