open Village
open Mapgen

let max_hex = 255
let min a b = if a < b then a else b

let get_ith_interval a b n i =
  let width = (b - a) / n in
  let reste = (b - a) mod n in
  let x = a + (i * width) in
  let y = min b (a + ((i + 1) * width)) in
  (x, if b - y <= width then y + reste else y)

(* *On comptabilise le nombre de constructeurs possibles pour chaque type *)
let n_arg = 2
let n_cond = 2

(* Création du dict des codes ressources *)
let ress_dict = Hashtbl.create 16
let () = Hashtbl.add ress_dict Nouriture 0
let () = Hashtbl.add ress_dict Main_d_oeuvre 1
let () = Hashtbl.add ress_dict Pierre 2
let () = Hashtbl.add ress_dict Wood 3
let () = Hashtbl.add ress_dict Lit 4

(* Place les clés dans un tableau avec leur valeur en indice *)
let ress_array = Utils.array_of_dict ress_dict

(* Création du dict des codes d'inégalités *)
let ing_brut_dict = Hashtbl.create 16
let () = Hashtbl.add ing_brut_dict EquivalentBrut 0
let () = Hashtbl.add ing_brut_dict MoinsBrut 1
let () = Hashtbl.add ing_brut_dict PlusBrut 2
let ing_brut_array = Utils.array_of_dict ing_brut_dict
let ing_percent_dict = Hashtbl.create 16
let () = Hashtbl.add ing_percent_dict LessPercent 0
let () = Hashtbl.add ing_percent_dict MorePercent 1
let ing_percent_array = Utils.array_of_dict ing_percent_dict

(* Création du dict des codes de biomes *)
let biome_dict = Hashtbl.create 16
let () = Hashtbl.add biome_dict Forest 0
let () = Hashtbl.add biome_dict Desert 1
let () = Hashtbl.add biome_dict Plains 2
let biome_array = Utils.array_of_dict biome_dict

(* Création du dict des codes des batiments *)
let bat_dict = Hashtbl.create 16
let () = Hashtbl.add bat_dict Maison 0
let () = Hashtbl.add bat_dict Carriere 1
let () = Hashtbl.add bat_dict Scierie 2
let () = Hashtbl.add bat_dict Ferme 3
let bat_array = Utils.array_of_dict bat_dict

(* Création du dict des codes des arguments *)
let arg_dict = Hashtbl.create 16
let () = Hashtbl.add arg_dict OutCity 0
let () = Hashtbl.add arg_dict InCity 1
let arg_array = Utils.array_of_dict arg_dict
let ress_coder ress = Hashtbl.find ress_dict ress
let ing_brut_coder ing = Hashtbl.find ing_brut_dict ing
let ing_percent_coder ing = Hashtbl.find ing_percent_dict ing
let biome_coder biome = Hashtbl.find biome_dict biome
let serialize_bat bat = Hashtbl.find bat_dict bat
let serialize_arg arg = Hashtbl.find arg_dict arg
let serialize_prio = function Random -> 0 | Pref b -> 1 + biome_coder b
let prio_of_code code = if code > 0 then Pref biome_array.(code) else Random

let code_to_int n code =
  let x, _ = get_ith_interval 0 max_hex n code in
  x

let int_to_code n code =
  n * code / max_hex

let n_ress = Hashtbl.length ress_dict
let n_ing_brut = Hashtbl.length ing_brut_dict
let n_ing_percent = Hashtbl.length ing_percent_dict
let n_biome = Hashtbl.length biome_dict
let n_arg = Hashtbl.length arg_dict
let n_bat = Hashtbl.length bat_dict
let n_prio = 1 + Hashtbl.length biome_dict

(* Pour n >= borne_sup, code_of_float n = 255 *)
let borne_sup = 1e4

(* Transforme un entier sur 64 bits en un code sur 8 bits  *)
let int_to_byte x =
  int_of_float
    (float_of_int max_hex *. (1. -. (1e18 ** (-.float_of_int x /. borne_sup))))

let byte_to_int byte =
  -int_of_float
     (borne_sup
     *. Float.log (-. (-.Float.log (float_of_int byte)) /. 255.)
     /. Float.log 1e18)

let serialize_cond condition =
  match condition with
  | InegaliteBrute (rss1, rss2, inegalite_brut, seuil) ->
      let () = print_string "cond_ress1->str: "; print_int (ress_coder rss1); print_char '\n' in
      [
        code_to_int 2 0;
        code_to_int n_ress (ress_coder rss1);
        code_to_int n_ress (ress_coder rss2);
        code_to_int n_ing_brut (ing_brut_coder inegalite_brut);
        int_to_byte seuil;
      ]
  | InegaliteEnPourcentage (rss1, rss2, ing_percent, seuil) ->
      [
        code_to_int 2 1;
        code_to_int n_ress (ress_coder rss1);
        code_to_int n_ress (ress_coder rss2);
        code_to_int n_ing_percent (ing_percent_coder ing_percent);
        int_to_byte seuil;
      ]

let int_to_hex n =
  let hex = Printf.sprintf "%X" n in
  if String.length hex < 2 then "0" ^ hex else hex

let rec int_list_to_hex_str l =
  match l with e :: q -> int_to_hex e ^ int_list_to_hex_str q | [] -> ""

let serialize_action action =
  let arg, bat, prio = action in
  [ serialize_arg arg; serialize_bat bat; serialize_prio prio ]

let rec hauteur tree =
  match tree with
  | Node (_, l_tree, r_tree, _) ->
      let l_hauteur = hauteur l_tree in
      let r_hauteur = hauteur r_tree in
      if l_hauteur < r_hauteur then 1 + r_hauteur else 1 + l_hauteur
  | Vide -> 0

let pere i = (i - 1) / 2
let fils_g i = (2 * i) + 1
let fils_d i = (2 * i) + 2

(* type tree = Vide | Node of condition * tree * tree * action *)

let serialize_tree tree =
  let h = hauteur tree in
  let serialization_array =
    Array.make (Utils.pow 2 (h + 1) - 1) "################"
  in
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
  serialization_array

(* Décodage des chaînes hex *)

(* Vérifie si le tableau de chaînes hexadéciamles
   possède des éléments de la vonne longueur *)
let check_length splitted =
  let b = ref true in
  let n = Array.length splitted in
  for i = 0 to n - 1 do
    if String.length splitted.(i) <> 16 then b := false
  done;
  !b

(* Découpe une chaîne hexadécimale en un tableau de mots de 8 octets *)
let hex_string_splitter hex =
  let n = String.length hex in
  let l = n / 16 in
  (* le nombre d'éléments *)
  let splitted = Array.make l "" in
  for i = 0 to n - 1 do
    splitted.(i / 16) <- splitted.(i / 16) ^ String.make 1 hex.[i]
  done;
  assert (check_length splitted);
  splitted

(* Le Noeud avec que des 0 est un noeud vide qui complète les arbres *)
let is_token_node hex_node =
  let n = Array.length hex_node in
  let b = ref true in
  for i = 0 to n - 1 do
    if hex_node.(i) <> -1 then b := false
  done;
  !b

let word_splitter word =
  let n = String.length word in
  let () = assert (n mod 2 = 0) in
  let splitted = Array.make (n / 2) 0 in
  for i = 0 to (n / 2) - 1 do
    let hex = String.make 1 word.[i] ^ String.make 1 word.[i + 1] in
    if hex <> "##" then 
      splitted.(i) <- int_of_string ("0x" ^ hex)
     else
      splitted.(i) <- -1
  done;
  splitted

let cond_of_noeud splitted_words =
  print_string "Dans cond_of_noeud\n";
  let cond_type = int_to_code 2 splitted_words.(0) in
  let cond_ress1 = int_to_code n_ress splitted_words.(1) in
  let cond_ress2 = int_to_code n_ress splitted_words.(2) in
  let cond_ing = splitted_words.(3) in
  let cond_seuil = splitted_words.(4) in
  let () = print_string "cond_ress1->noeud: "; print_int cond_ress1; print_char '\n' in
  if cond_type = 0 then
    let cond_ing_brut = 
      ing_brut_array.(int_to_code n_ing_brut cond_ing) in
    InegaliteBrute
      ( ress_array.(cond_ress1),
        ress_array.(cond_ress2),
        cond_ing_brut,
        byte_to_int cond_seuil )
  else
    let cond_ing_percent = 
      ing_percent_array.(int_to_code n_ing_percent cond_ing) in
    InegaliteEnPourcentage
      ( ress_array.(cond_ress1),
        ress_array.(cond_ress2),
        cond_ing_percent,
        byte_to_int cond_seuil )

let action_of_noeud splitted_words =
  print_string "Dans action_of_noeud\n";
  let action_arg = int_to_code n_arg splitted_words.(5) in
  let action_bat = int_to_code n_bat splitted_words.(6) in
  let action_prio = int_to_code n_prio splitted_words.(7) in
  let () =
    Printf.printf "arg: %d; bat: %d; prio: %d\n" action_arg action_bat
      action_prio
  in
  (arg_array.(action_arg), bat_array.(action_bat), prio_of_code action_prio)

let construire_arbre splitted =
  let hex_splitted = hex_string_splitter splitted in
  let n = Array.length hex_splitted in
  let rec construire_noeud i =
    if i >= n then Vide
    else
      let mot_hex = word_splitter hex_splitted.(i) in
      let s = String.concat "/" (Array.to_list (Array.map string_of_int mot_hex)) in
      let () = print_string s; print_string "\n" in
      if not (is_token_node mot_hex) then (
        print_string "Dans Node\n";
        Node
          ( cond_of_noeud mot_hex,
            construire_noeud (fils_g i),
            construire_noeud (fils_d i),
            action_of_noeud mot_hex ))
      else (
        print_string "Dans Vide\n";
        Vide)
  in
  construire_noeud 0
