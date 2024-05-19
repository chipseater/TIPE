open Interpolation

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
  let sum = ref 0 in
  let count = ref 0 in
  (* Cycles through the adjacent tiles and counts their number
     while summing their values to average them *)
  for k = 0 to 3 do
    let i_offset, j_offset = [| -1, 0 ; 1, 0 ; 0, -1 ; 0, 1 |].(k) in
    let new_i, new_j = i + i_offset, j + j_offset in
    if is_valid n new_i new_j 
      then (sum := !sum + map.(new_i).(new_j); 
      count := !count + 1)
  done;
  !sum / !count
;;

let average_map map =
  let n = Array.length map in
  let interpolated_map = Array.make_matrix n n 0 in
  for i = 0 to (n - 1) do
    for j = 0 to (n - 1) do
      interpolated_map.(i).(j) <- average_adjacent map n i j
    done
  done;
  interpolated_map
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

