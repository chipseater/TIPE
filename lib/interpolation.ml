(* Returns the sqared error between a 1-dimensional dataset of length n
   and a function f: int -> float *)
let sq_err data n a b = 
  let f x = a *. (float_of_int x) +. b in
  let error = ref 0. in
  for i = 0 to (n - 1) do
    let item = float_of_int data.(i) in
    error := !error +. (item -. (f i)) ** 2.
  done;
  !error
;;

(* Computes the first partial derivative of the error according to a and b *)
let sq_err_diff data n a b =
  (* h is the tiny nudge a and b will be increased by *)
  let h = 0.01 in
  let sq_err_f = sq_err data n a b in
  let sq_err_f_a = sq_err data n (a +. h) b in
  let sq_err_f_b = sq_err data n a (b +. h) in
  (* Computes the rate of change of the funciton according to a and b *)
  let sq_err_diff_a = (sq_err_f_a -. sq_err_f) /. h in
  let sq_err_diff_b = (sq_err_f_b -. sq_err_f) /. h in
  sq_err_diff_a, sq_err_diff_b
;;

(* Computes the second partial derivative of the error according to a and b *)
let sq_err_2diff data n a b =
  let h = 0.01 in
  (* Computes the error caused by diffrent interpolations 
     to approximate the second derivative of the sq_err *)
  let sq_err_f = sq_err data n a b in
  let sq_err_f1_a = sq_err data n (a +. h) b in
  let sq_err_f1_b = sq_err data n a (b +. h) in
  let sq_err_f2_a = sq_err data n (a +. 2. *. h) b in
  let sq_err_f2_b = sq_err data n a (b +. 2. *. h) in
  (* The second derivative can be computed as:
     lim h->0 of (f(x + 2h) - 2f(x + h) + f(x)) / h^2 *)
  let sq_err_diff2_a = 
    (sq_err_f2_a -. 2. *. sq_err_f1_a +. sq_err_f) /. h ** 2. in
  let sq_err_diff2_b = 
    (sq_err_f2_b -. 2. *. sq_err_f1_b +. sq_err_f) /. h ** 2. in
  sq_err_diff2_a, sq_err_diff2_b
;;

(* Returns the parameters of an affine interpolation of a 1d array 
   using newton's method to minimize the square of the error *)
let interpolate data steps =
  (* int -> float *)
  let n = Array.length data in
  let a, b = ref 0., ref 0. in
  for i = 1 to steps do 
    let a_diff, b_diff = sq_err_diff data n !a !b in
    let a_2diff, b_2diff = sq_err_2diff data n !a !b in
    a := !a -. a_diff /. a_2diff;
    b := !b -. b_diff /. a_2diff
  done;
  !a, !b
;;

