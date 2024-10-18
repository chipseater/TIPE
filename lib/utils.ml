let arr_cartesian_product arr1 arr2 =
  let n1 = Array.length arr1 in
  let n2 = Array.length arr2 in
  (* Using the first element as a placeholder *)
  let prod = Array.make (n1 * n2) (arr1.(0), arr2.(0)) in
  for i = 0 to n1 - 1 do
    for j = 0 to n2 - 1 do
      prod.(j + (n1 * i)) <- (arr1.(i), arr2.(j))
    done
  done;
  prod

let arr_cartesian_square arr = arr_cartesian_product arr arr

(* Calcule n^k *)
let rec pow n k =
  if k = 0 then 1
  else if k = 1 then n
  else if k mod 2 = 0 then pow n (k / 2) * pow n (k / 2)
  else n * pow n (k / 2) * pow n (k / 2)

let format_map_name ?(map_label = "map") map_number =
  map_label ^ string_of_int map_number ^ ".json"

(* Génère une distribution normale en utilisant l'algorithme de Box-Muller *)
let rand_normal mean std_deviation =
  let phi = 2.0 *. Float.pi *. Random.float 1.0 in
  let r = sqrt (-2. *. log (Random.float 1.0)) in
  mean +. (std_deviation *. r *. cos phi)

let int_rand_normal mean std_deviation =
  int_of_float (rand_normal (float_of_int mean) (float_of_int std_deviation))

(* Donne true avec une probabilité de p *)
let rand_bool p = Random.float (1. /. p) <= 1.
