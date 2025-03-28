open Village
open Mapgen

let max_hex = 255
let min a b = if a < b then a else b

let get_ith_interval a b n i =
  let width = (b - a) / n in
  let reste = (b - a) mod n in
  let x = a + i * width in
  let y = min b (a + (i + 1) * width) in
  (min 1 x, if (b - y) <= width then y + reste else y)

(* *On comptabilise le nombre de constructeurs possibles pour chaque type *)
let n_arg = 2
let n_cond = 2

let ress_coder ress =
  match ress with
  | Nouriture -> 0
  | Main_d_oeuvre -> 1
  | Pierre -> 2
  | Wood -> 3
  | Lit -> 4

(* type inegalite_brut = PlusBrut | MoinssBrut | EquivalentBrut *)
let ing_brut_coder ing =
  match ing with
  | EquivalentBrut -> 0
  | MoinsBrut -> 1
  | PlusBrut -> 2

(* type inegalite_brut = PlusBrut | MoinssBrut | EquivalentBrut *)
let ing_percent_coder ing =
  match ing with
  | LessPercent -> 0
  | MorePercent -> 1

let code_to_int n code =
  let x, _ = get_ith_interval 0 max_hex n code in x

(* Pour n >= borne_sup, code_of_float n = 255 *)
let borne_sup = 1e4
(* Transforme un entier sur 64 bits en un code sur 8 bits  *)
let int_to_byte x =
  int_of_float ((float_of_int max_hex)
   *.(1. -. 1e18 ** (-.(float_of_int x) /. borne_sup)))

let n_ress = 5
let n_ing_brut = 3
let n_ing_percent = 2

let serialize_cond condition =
  match condition with
  | InegaliteBrute (rss1, rss2, inegalite_brut, seuil) ->
    [code_to_int 2 0; code_to_int n_ress (ress_coder rss1); code_to_int n_ress (ress_coder rss2); code_to_int n_ing_brut (ing_brut_coder inegalite_brut);
    int_to_byte seuil]
  | InegaliteEnPourcentage (rss1, rss2, ing_percent, seuil) ->
    [code_to_int 2 1; code_to_int n_ress (ress_coder rss1); code_to_int n_ress (ress_coder rss2);code_to_int n_ing_percent (ing_percent_coder ing_percent);
    int_to_byte seuil]

let biome_coder = function
  | Forest -> 0
  | Desert -> 1
  | Plains -> 2

let serialize_prio = function
  | Random -> 0
  | Pref b -> 1 + biome_coder b

let serialize_bat = function
  | Maison -> 0
  | Carriere -> 1
  | Scierie -> 2
  | Ferme -> 3

let serialize_arg = function
  | OutCity -> 0 | InCity -> 1

let int_to_hex n =
  let hex = Printf.sprintf "%X" n in
  if String.length hex < 2 then ("0" ^ hex)
  else hex

let rec int_list_to_hex_str l =
  match l with
  | e :: q -> (int_to_hex e) ^ int_list_to_hex_str q
  | [] -> ""

let serialize_action action =
  let arg, bat, prio = action in
  [serialize_arg arg; serialize_bat bat; serialize_prio prio]

let rec serialize_tree_fun tree =
  match tree with
  | Node (condition, l_tree, r_tree, action)
    -> (int_list_to_hex_str (serialize_cond condition))
        ^ (serialize_tree_fun l_tree)
        ^ (serialize_tree_fun r_tree)
        ^ (int_list_to_hex_str (serialize_action action))
  | Vide -> "G"


let rec hauteur tree =
  match tree with
  | Node (_, l_tree, r_tree, _) ->
      let l_hauteur = hauteur l_tree in
      let r_hauteur = hauteur r_tree in
      if (l_hauteur < r_hauteur)
        then 1 + r_hauteur
        else 1 + l_hauteur
  | Vide -> 0

let pere i = (i - 1) / 2
let fils_g i = 2 * i + 1
let fils_d i = 2 * i + 2

(* type tree = Vide | Node of condition * tree * tree * action *)

let serialize_tree tree =
  let h = hauteur tree in
  let serialization_array = 
    Array.make (Utils.pow 2 (h + 1) - 1) "00000000000000000" in
  let rec serializer t i =
    match t with
    | Node (cond, t_g, t_d, action) ->
        serialization_array.(fils_g i) <- serializer t_g (fils_g i);
        serialization_array.(fils_d i) <- serializer t_d (fils_d i);
        int_list_to_hex_str (serialize_cond cond)
          ^ int_list_to_hex_str (serialize_action action)
    | Vide -> ""
  in
  serialization_array.(0) <- serializer tree 0;
  String.concat "" (Array.to_list serialization_array)


let hex_string_splitter hex =
  let n = Array.length hex in
  let l = n / 16 in (* le nombre d'éléments *)
  let splitted = Array.make l "" in
  for i = 0 to n do
    splitted.(i / l) <- hex.(i)
  done;
  splitted
