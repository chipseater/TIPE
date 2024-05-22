let z_max = 9;;
let chunk_width = 4;;

type biome = Forest | Desert | Plains
type building = House | Quarry | Sawmill | Farm 

(* A tile is made out of the eventual building it 
   contains associated with its elevation *)
type tile = (building option) * int

(* A chunk is a 4*4 tile matrix associated with its biome *)
type chunk = ((tile array) array) * biome 
(* A n*n map is a n/4*n/4 chunk matrix *)
type map = ((chunk array) array)

(* Contains the pole coordinates and biome *)
type pole = int * int * biome

(* Sets the balance between the diffrent biomes,
   here 8 plains for 1 desert and 1 tundra *)
let int_to_biome b =
  assert (b >= 0 && b < 10);
  if (b < 5) then Plains
  else if (b = 5) then Desert
  else Forest

(* Generates k random poles between 0 and n - 1 *)
let rec gen_poles n k =
  let () = Random.self_init () in
  if (k = 0) then []
  else let pole_biome = int_to_biome (Random.int 10) in
  (Random.int n, Random.int n, pole_biome)::(gen_poles n (k - 1))

(* Returns the euclidian distance between p2 and p1 *)
let distance p1 p2 = 
  let x1, y1 = p1 in
  let x2, y2 = p2 in
  (Stdlib.sqrt (float_of_int ((x2 - x1)*(x2 - x1) + (y2 - y1)*(y2 - y1))))

(* Returns the nearest pole from the p point *)
let rec nearest_biome p poles = match poles with
  | [] -> raise (Invalid_argument "Empty list")
  | [(_, _, b)] -> b
  (* Removes the farthest distance from the pole list *)
  | (x1, y1, b1)::(x2, y2, b2)::q ->
      nearest_biome p ((if ((distance (x1, y1) p) < (distance (x2, y2) p)) 
        then (x1, y1, b1) else (x2, y2, b2))::q)

(* Generates a nxn sized map with k biomes *)
 let gen_map n k =
  let map = Array.make_matrix n n Plains in
  let poles = gen_poles n k in
  for i = 0 to (n - 1) do
    for j = 0 to (n - 1) do
      map.(i).(j) <- nearest_biome (i, j) poles
  done
  done; map
;;

(* Returns a nxn grid of random numbers *)
let gen_random_values n =
  let () = Random.self_init () in
  let map = Array.make_matrix n n 0 in
  for i = 0 to (n - 1) do
    for j = 0 to (n - 1) do
      map.(i).(j) <- (Random.int z_max)
    done
  done;
  map
;;

(* Checks if the point is not outside of a nxn map *)
let is_valid n i j = not (i < 0 || i >= n || j < 0 || j >= n);;

(* Returns the average of the neighbouring square in a nxn map *)
let average_adjacent map n i j =
  let sum = ref 0. in
  let count = ref 0. in
  (* Cycles through the adjacent tiles and counts 
     their number while summing their values to average them *)
  for k = 0 to 3 do
    let i_offset, j_offset = 
      [| -1, 0 ; 1, 0 ; 0, -1 ; 0, 1 |].(k) in
    let new_i, new_j = i + i_offset, j + j_offset in
    if is_valid n new_i new_j 
      then (sum := !sum +. map.(new_i).(new_j); 
      count := !count +. 1.)
  done;
  !sum /. !count
;;

let average_map map =
  let n = Array.length map in
  let interpolated_map = Array.make_matrix n n 0. in
  for i = 0 to (n - 1) do
    for j = 0 to (n - 1) do
      interpolated_map.(i).(j) <- average_adjacent map n i j
    done
  done;
  interpolated_map
;;

(* Generates a (n / grid_width)^2 grid with 
   a random noramlized vector at each node *)
let gen_rand_grad n grid_width =
  let () = Random.self_init () in
  let grad_grid = 
    Array.make_matrix (n / grid_width) (n / grid_width) (0., 0.) in
  for i = 0 to (n / grid_width - 1) do
    for j = 0 to (n / grid_width - 1) do
      let rand_angle = 
        (float_of_int (Random.int 720)) *. Float.pi /. 360. in
      grad_grid.(i).(j) <- cos rand_angle, sin rand_angle
    done
  done;
  grad_grid
;;

let smoothstep x = 
  -. 20. *. x ** 7. +. 70. *. x ** 6. -. 84. *. x ** 5. +. 35. *. x ** 4. ;;
(* let smoothstep x = x;; *)

(* Gives a smooth appearance to the noise *)
let interpolate a b x = 
  if x < 0. then 0. else if x > 1. then 1.
  else (b -. a) *. (smoothstep x) +. a
;;

let local_coord x n =
  let frac x = x -. Float.floor x in
  frac ((float_of_int x) /. (float_of_int n))
;;

let perlin grad_grid grid_width n i j =
  (* The local coordinates in the grid cells *)
  let li, lj = local_coord i grid_width, local_coord j grid_width in
  (* The bottom-left coner coordinates of the grid cell *)
  let tl_i_corner, tl_j_corner = i / grid_width, j / grid_width in
  (* Gets each gradient vector at each corner of the grid cell *)
  let tl_grad_i, tl_grad_j = grad_grid.(tl_i_corner).(tl_j_corner) in
  let tr_grad_i, tr_grad_j = grad_grid.(tl_i_corner).(tl_j_corner + 1) in
  let bl_grad_i, bl_grad_j = grad_grid.(tl_i_corner + 1).(tl_j_corner) in
  let br_grad_i, br_grad_j = grad_grid.(tl_i_corner + 1).(tl_j_corner + 1) in
  (* Computes the dot product between the local coordinates
     and the gradient vector at each corner *)
  let tl_dot_prod = li *. tl_grad_j +. lj *. tl_grad_i in
  let tr_dot_prod = li *. tr_grad_j +. (lj -. 1.) *. tr_grad_i in
  let bl_dot_prod = (li -. 1.) *. bl_grad_j +. lj *. bl_grad_i in
  let br_dot_prod = (li -. 1.) *. br_grad_j +. (lj -. 1.) *. br_grad_i in
  (* Interpolates the dot products from left to right 
     then from bottom to top *)
  (* print_float li; print_char ' '; print_float lj; print_char '\n';
  print_float li; print_char ' '; print_float (1. -. lj); print_char '\n';
  print_float (1. -. li); print_char ' '; print_float lj; print_char '\n';
  print_float (1. -. li); print_char ' '; print_float (1. -. lj); print_char '\n'; *)
  let top_interpolation = interpolate tl_dot_prod tr_dot_prod lj in
  let bottom_interpolation = interpolate bl_dot_prod br_dot_prod lj in
  interpolate top_interpolation bottom_interpolation li
;;

let perlin_layer map n total grid_width =
  let grad_grid = gen_rand_grad (n + 2 * grid_width) grid_width in
  for i = 0 to (n - 1) do
    for j = 0 to (n - 1) do 
      map.(i).(j) <-
        map.(i).(j) +. (0.5 *. (perlin grad_grid grid_width n i j) +. 0.5) /. (float_of_int total)
    done
  done
;;

let perlin_map n grid_width octaves =
  let map = Array.make_matrix n n 0. in
  let period = ref 1 in
  for _ = 1 to octaves do
    period := !period * 2;
    perlin_layer map n octaves (grid_width / !period)
  done;
  map
;;

let print_int_map map =
  let n = Array.length map in
  for i = 0 to (n - 1) do
    for j = 0 to (n - 1) do
      print_int map.(i).(j);
      print_char ' '
    done;
    print_char '\n'
  done;
;;

let print_float_map map =
  let n = Array.length map in
  for i = 0 to (n - 2) do
    for j = 0 to (n - 2) do
      print_float map.(i).(j);
      print_char ' '
    done;
    print_float map.(i).(n - 1);
    print_char '\n'
  done;
;;

let print_biome_map map =
  let n = Array.length map in
  for i = 0 to (n - 1) do
    for j = 0 to (n - 1) do
      match map.(i).(j) with
      | Forest -> print_string "F "
      | Desert -> print_string "D "
      | Plains -> print_string "P ";
    done;
    print_char '\n'
  done;
;;

let grads = gen_rand_grad 100 10;;
perlin grads 10 10 9;; 

