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

let a = arr_cartesian_square [| 1; 2; 3; 4 |]
let n = Array.length a
