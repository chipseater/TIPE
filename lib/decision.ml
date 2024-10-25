open Village
open Mapmanage
open Mapgen








let aff arr = 
  let n = Array.length arr in
  print_char '\n';
  let m = Array.length arr.(1) in
  for i = 0 to (n-1) do 
    for j =0 to (m-1) do 
      print_int (arr.(i).(j));print_char '\t' 
    done ;
    print_char '\n'
  done
let rec aff2 tab = 
  match tab with 
  |e::q -> let a,b = e in print_int a;print_char ' ' ; print_int b ;print_char '\n';  aff2 q
  |[]->()
  












(* Vérifie si un noeud est vide *)
let isEmpty = function Vide -> true | _ -> false

(* Test si la ressource 1 suppérieur ou inférieur à la ressource 2 selon l'ingalité et si le pourcentage est inférieur à la diférence *)
let ingpercent r1 r2 ing pourcent donnee : bool =
  let nr1 = search donnee r1 in
  let nr2 = search donnee r2 in
  match ing with
  | More ->
      if nr1 = 0 then true else
      let ratio = nr2 * 100 / nr1 in
      if nr1 > nr2 then ratio > pourcent else false
  | Less ->
      if nr1 = 0 then false else
      let ratio =  nr2 * 100 / nr1 in
      if nr1 < nr2 then ratio > pourcent else false
  | Equal -> false

(* Test si la ressource n1 suppérieur ou inférieur à la ressource 2 selon l'ingalité et si le minimum est inférieur à la diférence *)
let ingflat r1 r2 ing min donnee : bool =
  let nr1 = search donnee r1 in
  let nr2 = search donnee r2 in
  match ing with
  | More ->
      let dif = nr1 - nr2 in
      if nr1 > nr2 then dif > min else false
  | Less ->
      let dif = nr1 - nr2 in
      if nr1 < nr2 then -dif > min else false
  | Equal ->
      abs (nr2 - nr1) < min

(* Effectue le test selon l'objet *)
let test (donnee : data) (condition : condition) : bool =
  match condition with
  | Ingpercent (r1, r2, ing, pourcent) -> ingpercent r1 r2 ing pourcent donnee
  | Ingflat (r1, r2, ing, min) -> ingflat r1 r2 ing min donnee

 

(* Teste s' il y a une tuile du chunk qui est vide *)
let test_not_full (chunk : chunk) : bool =
  match chunk with
  | None -> failwith "Not a Chunk"
  | Chunk (x, _) ->
      let t = ref false in
      for i = 0 to chunk_width - 1 do
        for j = 0 to chunk_width - 1 do
          let (Tile (c, _)) = x.(i).(j) in
          if c = None then t := true
        done
      done;
      !t

(* Ajoute dans un tableau toutes les cases qui sont constructibles *)
let possibilite chunk =
  let arr = Array.make 16 (-1, -1) in
  print_char 'n';
  let tab = get_chunk_tiles chunk in
  for i = 0 to chunk_width - 1 do
    for j = 0 to chunk_width - 1 do
      let (Tile (b, _)) = tab.(i).(j) in
      (* print_int !c; *)
      if b = None then arr.(i*4+j) <- (i, j);
    done ; 
    (* print_char 'm' *)
  done;
  print_char '\n';
  let li = Array.to_list arr in 
  aff2 li;
  print_char '\n';  
  arr

(* Place le batiment dans un des chunks  *)
let buildtile (build : building) (map : map) (table : (int * int) array) 
 (village:village) =
  Array.shuffle ~rand:Random.int table;
  let x, y = table.(0) in
  let temp = map.(x).(y) in
  village.position_list <- (x,y) :: village.position_list;
  let arr = possibilite temp in
  Array.shuffle ~rand:Random.int arr;
  print_char 'f';
  print_char '\n';
  print_int (Array.length arr);
  let rec choice arr c =
    print_int c;
    match arr.(c) with
    | -1, -1 ->  (choice arr (c + 1))
    | i, j ->   print_char '\n';print_char 'b';mutate_building_in_chunk map.(x).(y) (Some build) i j;print_char 'b'
  in
  choice arr 0
;;

let nul table map = 
  let n = Array.length table in
  let o = ref 0 in 
  try  
  for t=0 to n-1 do
    o := t ;
    let x, y = table.(t) in
    if (test_not_full (map.(x).(y))) then raise Exit
  done;
  raise Not_found
  with Exit -> table.(!o)
  |Not_found -> (-1,-1)
  





  (* Place le batiment dans un des chunks  *)
let build_tile_in (build : building) (map : map) (table : (int * int) array) =
  print_char 'A';
  Array.shuffle ~rand:Random.int table;
  let (x,y) = nul table map in (*A voir*)
  if x = -1 then () else
  let temp = map.(x).(y) in 
  let arr = possibilite temp in
  Array.shuffle ~rand:Random.int arr;
  print_int x;
  print_int y;
  print_char '\n';
  let li = Array.to_list arr in 
  aff2 li;
  let rec choice arr c =
    print_int c;
    match arr.(c) with
    | -1, -1 -> choice arr (c + 1)
    | i, j -> mutate_building_in_chunk map.(x).(y) (Some build) i j
  in
  choice arr 0

(* Calcule la taille et la position en haut à gauche du tableau *)
let pos_card (pos_list : position list) =
  match pos_list with
  | [] -> failwith "No Chunk"
  | a :: _ ->
      let x, y = a in
      let top, left, right, bot = (ref x, ref y, ref y, ref x) in
      (* corner, largeur, hauteur *)
      let rec parc (pos_list : position list) =
        match pos_list with
        | [] ->
            let b, d, e, g = (!left, !right, !top, !bot) in
            ((e - 1, b - 1), d - b + 3, g - e + 3)
        | (a, b) :: q ->
            if a > !bot then bot := a;
            if a < !top then top := a;
            if b < !left then left := b;
            if b > !right then right := b;
            parc q
      in
      parc pos_list


let rec proxi (arr : int array array) (pos_list : position list) (world_limit:int)
  (corner : position) =
  let pas_valid n i j = (i < 0 || i >= n || j < 0 || j >= n) in 
  let p, m = corner in
  (* J'aurais pu mettre [| (-1, -1), (-1, 0), ..., (1, 1) |] *)
  let range = Utils.arr_cartesian_square [| -1; 0; 1 |] in
  match pos_list with
  | [] -> ()
  | (x, y) :: q ->
      for i = 0 to 8 do
        (* Prends successivement les 9 positions adjacentes à (0, 0) *)
        let r_x, r_y = range.(i) in 
        if (pas_valid world_limit (x+r_x) (y+r_y)) then arr.(x - p + r_x).(y - m + r_y) <- -100
        else if ((r_x, r_y) <> (0, 0)) then
          arr.(x + r_x - p).(y + r_y - m) <- arr.(x + r_x - p).(y + r_y - m) + 1
        else
          arr.(x - p).(y - m) <- arr.(x - p).(y - m) -10;
      done;
      proxi arr q world_limit corner



(* Parcours la matrice pour lister les positions les plus probables *)
let parc_mat (arr : int array array) (h : int) (l : int) (corner : int * int) (map:map) =
  let a, b = corner in
  let c = ref 0 in
  let list = ref [] in
  for i = 0 to h - 1 do
    for j = 0 to l - 1 do
      print_char 'x';
      if (arr.(i).(j) > !c) && (test_not_full (map.(i+a).(j+b))) then (
        list := [ (i+a, j+b) ];
        c := arr.(i).(j))
      else if arr.(i).(j) = !c && test_not_full (map.(i+a).(j+b)) then list := (i + a, j + b) :: !list
      else ()
    done
  done;
  aff arr;
  print_char 'm';
  aff2 !list;
  print_char 'n';
  !list

(* Construit le batiment à l'extérieur du village sans biome privilegié *)
let r_buildout (build : building) (map : map) (pos_list : position list)  (village:village) =
  (let coner, larg, haut = pos_card pos_list in
  let mat = Array.make_matrix haut larg 0 in
  let world_limit = Array.length map in
  proxi mat pos_list world_limit coner;  
  print_char 'y';
  let list = parc_mat mat haut larg coner map in
  print_char 'y';
  let arr = Array.of_list list in
  print_char 'y';
  buildtile build map arr  village
  )
(* Construit le batiment à l'intérieur du village sans biome privilegié *)
let r_buildin (build : building) (map : map) (pos_list : position list) (village:village) : unit =
  let rec empile (pos_list : position list) : (int * int) list =
    match pos_list with
    | [] -> []
    | (x, y) :: q when test_not_full map.(x).(y) = true -> empile q
    | (x, y) :: q -> (x, y) :: empile q
  in
  let temp = empile pos_list in
  match temp with
  | [] ->  print_char 'h';r_buildout build map pos_list village
  | _ :: _ ->
      print_char 'g';
      let tab = Array.of_list temp in
      build_tile_in build map tab

(* Classe la liste en deux listes qui regroupe ceux du biome privilégier et les autres dans un autre *)
let classif (list : (int * int) list) (map : map) (biome : biome) =
  let rec parc l1 l2 l3 =
    match l1 with
    | (a, b) :: q when get_chunk_biome map.(a).(b) = biome && test_not_full map.(a).(b) ->
        parc q ((a, b) :: l2) l3
    | (a, b) :: q when test_not_full map.(a).(b) -> parc q l2 ((a, b) :: l3)
    | _::q -> parc q l2 l3 
    | [] -> (l2, l3)
  in
  parc list [] []

(* Construit le batiment à l'extérieur du village avec un biome privilegié *)
let pref_buildout (build : building) (map : map) (pos_list : position list)
    (biome : biome)  village : unit =
  (let corner, larg, haut = pos_card pos_list in
  (* print_char 'a'; *)
  let world_limit = Array.length map in
  let mat = Array.make_matrix haut larg 0 in
  (* print_char 'c'; *)
  proxi mat pos_list world_limit corner;
  (* print_char 'd'; *)
  let list = parc_mat mat haut larg corner map in
  (* print_char 'e'; *)
  let pref, autre = classif list map biome in
  (* print_char 'f'; *)
  match pref with
  | [] ->
      let arr = Array.of_list autre in
      (* print_char 'g'; *)
      buildtile build map arr village
  | _ ->
      let arr = Array.of_list pref in
      (* print_char 'h'; *)
      buildtile build map arr village)

(* Construit le batiment à l'intérieur du village avec un biome privilegié *)
let pref_buildin (build : building) (map : map) (pos_list : position list)
    (biome : biome) (village:village) =
  let rec empile (pos_list : position list) : (int * int) list =
    match pos_list with
    | [] -> []
    | (x, y) :: q when test_not_full map.(x).(y) = true -> empile q
    | (x, y) :: q -> (x, y) :: empile q
  in
  let temp = empile pos_list in
  let pref, autre = classif temp map biome in
  match pref with
  | [] -> (
      match autre with
      | [] -> pref_buildout build map pos_list biome village
      | _ :: _ ->
          let tab = Array.of_list autre in
          build_tile_in build map tab)
  | _ :: _ ->
      let tab = Array.of_list pref in
      build_tile_in build map tab

(* Effectue le type de construonction en fonction des paramètres *)
let to_do (action : action) (map : map) (pos_list : position list) (village:village) : unit =
  let arg, build, pref = action in
  if pref = Random then
    match arg with
    | InCity ->  print_char 'c';print_char 'w';r_buildin build map pos_list village
    | OutCity -> print_char 'd';print_char 'w';r_buildout build map pos_list  village
  else
    match pref with
    | Pref a -> (
        match arg with
        | InCity -> print_char 'e';print_char 'w'; pref_buildin build map pos_list a village
        | OutCity -> print_char 'f';print_char 'w'; pref_buildout build map pos_list a  village)
    | _ -> failwith "No other possibility"
  ;;
(* Evalue un noeud et fait ce qu'il faut *)
let rec eval_node (node : tree) (map : map) (village:village) =
  let (ressource,_) = village.logistics in 
  let pos_list = village.position_list in 
  print_char 'C';
  match node with
  | Vide -> failwith "Empty node"
  | Node (cond, sub_tree_left, sub_tree_right, action) ->  
    begin 
      if isEmpty sub_tree_left && test ressource cond then to_do action map pos_list  village 
      else if isEmpty sub_tree_right && not (test ressource cond) then to_do action map pos_list  village
      else if test ressource cond then eval_node sub_tree_left map village
      else eval_node sub_tree_right map  village
    end
