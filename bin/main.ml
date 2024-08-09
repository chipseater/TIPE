open Mapgen
open Mapmanage
open Decision
open Village
open Dumpmap

let stock_exp : data =
  [ (Bed, 0); (Food, 0); (People, 0); (Stone, 0); (Wood, 0) ]

let prod_exp : data =
  [ (Bed, 0); (Food, 100); (People, -25); (Stone, 0); (Wood, 0) ]

let mock_chunk1 =
  Chunk
    ( [|
        [|
          Tile (None, 10);
          Tile (None, 11);
          Tile (Some House, 12);
          Tile (None, 13);
        |];
        [|
          Tile (None, 9);
          Tile (None, 10);
          Tile (Some Quarry, 11);
          Tile (None, 12);
        |];
        [|
          Tile (None, 10); Tile (None, 10); Tile (Some Farm, 11); Tile (None, 11);
        |];
        [|
          Tile (None, 11);
          Tile (None, 10);
          Tile (Some House, 12);
          Tile (None, 10);
        |];
      |],
      Forest )

let mock_chunk2 =
  Chunk
    ( [|
        [| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];
        [| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];
        [| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];
        [| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |];
      |],
      Forest )

let mock_chunk3 =
  Chunk
    ( [|
        [|
          Tile (None, 10); Tile (Some Farm, 11); Tile (None, 12); Tile (None, 13);
        |];
        [|
          Tile (None, 9);
          Tile (Some Farm, 10);
          Tile (Some House, 11);
          Tile (None, 12);
        |];
        [|
          Tile (None, 10);
          Tile (Some Farm, 10);
          Tile (Some House, 11);
          Tile (None, 11);
        |];
        [|
          Tile (None, 11); Tile (Some Farm, 10); Tile (None, 12); Tile (None, 10);
        |];
      |],
      Forest )

let mock_map =
  [| [| mock_chunk2; mock_chunk2 |]; [| mock_chunk1; mock_chunk3 |] |]
;;

assert (sum_data stock_exp prod_exp = prod_exp);;

assert (
  get_production_from_tile (get_chunk_tiles mock_chunk1).(0).(1) = stock_exp)
;;

assert (
  get_production_from_tile (get_chunk_tiles mock_chunk1).(0).(2)
  = house_data_prodution)
;;

let data_1 =
  sum_data house_data_prodution
    (sum_data house_data_prodution
       (sum_data quarry_data_prodution farm_data_prodution))
in
assert (sum_chunk_production mock_chunk1 = data_1)
;;

let data_1 =
  sum_data house_data_prodution
    (sum_data house_data_prodution
       (sum_data quarry_data_prodution farm_data_prodution))
in
let pos_li = [ (0, 0); (1, 0) ] in
assert (sum_chunk_list_production pos_li mock_map = data_1)
;;

assert (update_logistics (stock_exp, prod_exp) = (prod_exp, void_data))

let stock_exp1 : data =
  [ (Bed, 40); (Food, 4); (People, 35); (Stone, 0); (Wood, 0) ]

let prod_exp1 : data =
  [ (Bed, 0); (Food, 10); (People, -25); (Stone, 0); (Wood, 0) ]

let result = [ (Bed, 0); (Food, 10); (People, 14); (Stone, 0); (Wood, 0) ];;

assert (update_all_logistics (stock_exp1, prod_exp1) = (result, void_data));;

let pos_li = [ (0, 0); (1, 0) ] in
let res_map =
  [| [| mock_chunk2; mock_chunk2 |]; [| mock_chunk2; mock_chunk3 |] |]
in

let paf = destroy_build (stock_exp, prod_exp) pos_li mock_map in
assert (paf = (stock_exp, stock_exp));
assert (mock_map = res_map)
(* 
;;;;
let aff arr = let n = Array.length arr in 
for i = 0 to (n-1) do 
  print_int (arr.(i))
done
;;
(*
let a = [|1;2;3;4 |] in 
shuffle a ;
aff a 
 *)
;;
let aff arr = 
  let n = Array.length arr in
  let m = Array.length arr.(1) in
  for i = 0 to (n-1) do 
    for j =0 to (m-1) do 
      print_int (arr.(i).(j)) 
    done ;
    print_char '\n'
  done
;;
let arr = [|[|0;0;0;0;0;0;0;0|];[|0;0;0;0;0;0;0;0|];[|0;0;0;0;0;0;0;0|];[|0;0;0;0;0;0;0;0|];[|0;0;0;0;0;0;0;0|];[|0;0;0;0;0;0;0;0|];[|0;0;0;0;0;0;0;0|];[|0;0;0;0;0;0;0;0|]|] in 
proxi arr [(1,1);(1,2)] (0,0);
aff arr ;
let (a,b),c,d = pos_card [(1,1);(1,2)] in 
print_int a;
print_int b;
print_int c;
print_int d;

print_char '\n'
;
print_char '\n'
;
print_char '\n'
;
let rec af a =match a with 
| (a,b) :: q ->print_int a;print_int b;
print_char '\n'
;af q
| [] -> () 
in
af (parc_mat arr d c ) 
;; 
 *)
;;

let stock_exp1 : data =
  [ (Bed, 40); (Food, 4); (People, 35); (Stone, 0); (Wood, 0) ]
in
let prod_exp1 : data =
  [ (Bed, 0); (Food, 10); (People, -25); (Stone, 0); (Wood, 0) ]
in
assert ( ingpercent Food Bed More 10 prod_exp1 =true); 
assert ( ingpercent Food Bed Less 1 stock_exp1 =true); 

let stock_exp1 : data =
  [ (Bed, 40); (Food, 4); (People, 35); (Stone, 0); (Wood, 0) ]
in

assert ( ingpercent Food Bed Less 80 stock_exp1 =true);

;;

let stock_exp1 : data =
  [ (Bed, 40); (Food, 4); (People, 35); (Stone, 0); (Wood, 0) ]
in

assert ( ingflat Food Bed Less 8 stock_exp1 =true);

;;


let stock_exp1 : data =
  [ (Bed, 40); (Food, 4); (People, 35); (Stone, 0); (Wood, 0) ]
in

assert ( ingflat Bed Food More 8 stock_exp1 =true);

;;

let stock_exp1 : data =
  [ (Bed, 40); (Food, 4); (People, 35); (Stone, 0); (Wood, 0) ]
in

assert ( equalpercent Bed Food 90 stock_exp1 =true);

;;

let stock_exp1 : data =
  [ (Bed, 40); (Food, 4); (People, 35); (Stone, 0); (Wood, 0) ]
in

assert ( equalflat Bed Food 42 stock_exp1 =true);

;;


let mock_chunk2 =
  Chunk
    ( [|
        [| Tile (Some Farm, 10); Tile (Some Farm, 11); Tile (Some Farm, 12); Tile (Some Farm, 13) |];
        [| Tile (Some Farm, 9); Tile (Some Farm, 10); Tile (Some Farm, 11); Tile (Some Farm, 12) |];
        [| Tile (Some Farm, 10); Tile (Some Farm, 10); Tile (Some Farm, 11); Tile (Some Farm, 11) |];
        [| Tile (Some Farm, 11); Tile (Some Farm, 10); Tile (Some Farm, 12); Tile (None, 10) |];
      |],
      Forest )
    in 
    assert (test_not_full mock_chunk2 = true );
  
  ;; 
  let mock_chunk2 =
    Chunk
      ( [|
          [| Tile (Some Farm, 10); Tile (Some Farm, 11); Tile (Some Farm, 12); Tile (Some Farm, 13) |];
          [| Tile (Some Farm, 9); Tile (Some Farm, 10); Tile (Some Farm, 11); Tile (Some Farm, 12) |];
          [| Tile (Some Farm, 10); Tile (Some Farm, 10); Tile (Some Farm, 11); Tile (Some Farm, 11) |];
          [| Tile (Some Farm, 11); Tile (Some Farm, 10); Tile (Some Farm, 12); Tile (None, 10) |];
        |],
        Forest )
      in 
      assert (possibilite mock_chunk2 = [|(-1,-1);(-1,-1);(-1,-1);(-1,-1);(-1,-1);(-1,-1);(-1,-1);(-1,-1);(-1,-1);(-1,-1);(-1,-1);(-1,-1);(-1,-1);(-1,-1);(-1,-1);(3,3)|] );
    
    ;;

    let mock_map2 =
      [| [| Chunk
        ( [|
            [| Tile (Some Farm, 10); Tile (Some Farm, 11); Tile (Some Farm, 12); Tile (Some Farm, 13) |];
            [| Tile (Some Farm, 9); Tile (Some Farm, 10); Tile (Some Farm, 11); Tile (Some Farm, 12) |];
            [| Tile (Some Farm, 10); Tile (Some Farm, 10); Tile (Some Farm, 11); Tile (Some Farm, 11) |];
            [| Tile (Some Farm, 11); Tile (Some Farm, 10); Tile (Some Farm, 12); Tile (None, 10) |];
          |],
          Forest )|]|]
    in
  buildtile Farm mock_map2 [|0,0|] ;
  assert (mock_map2=(      [| [| Chunk
  ( [|
      [| Tile (Some Farm, 10); Tile (Some Farm, 11); Tile (Some Farm, 12); Tile (Some Farm, 13) |];
      [| Tile (Some Farm, 9); Tile (Some Farm, 10); Tile (Some Farm, 11); Tile (Some Farm, 12) |];
      [| Tile (Some Farm, 10); Tile (Some Farm, 10); Tile (Some Farm, 11); Tile (Some Farm, 11) |];
      [| Tile (Some Farm, 11); Tile (Some Farm, 10); Tile (Some Farm, 12); Tile (Some Farm, 10) |];
    |],
    Forest )|]|]))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(*
  let mock_chunk1 =
    Chunk
      ( [|
          [| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];
          [| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];
          [| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];
          [| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |];
        |],
        Desert )
  in

 
  let mock_chunk2 =
    Chunk
      ( [|
          [| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];
          [| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];
          [| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];
          [| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |];
        |],
        Forest )
  in
  let mock_map =
    [|[|mock_chunk1;mock_chunk2;mock_chunk2|]; 
      [|mock_chunk2;mock_chunk1;mock_chunk2|];
      [|mock_chunk2;mock_chunk2;mock_chunk2|]|]
  in
  let printi a = match a with
  | b,n -> print_int b ; print_char ' ' ; print_int n ;print_char '\n'
  in
  let rec print (l1,l2) = match (l1,l2) with
  | e::q,_ -> (printi e ; print (q,l2); print_char '1')
  |[],e::q -> (printi e ; print ([],q); print_char '2')
  |[],[] -> ()
in (* print (classif [(0,0);(1,0);(1,1)] mock_map Desert ); *)
assert (([(1,1);(0,0)],[(1,0)])=classif [(0,0);(1,0);(1,1)] mock_map Desert )
*)
;;

let mock_chunk1 = Chunk
( [|
    [| Tile (Some Farm, 10); Tile (Some Farm, 11); Tile (Some Farm, 12); Tile (Some Farm, 13) |];
    [| Tile (Some Farm, 9); Tile (Some Farm, 10); Tile (Some Farm, 11); Tile (Some Farm, 12) |];
    [| Tile (Some Farm, 10); Tile (Some Farm, 10); Tile (Some Farm, 11); Tile (Some Farm, 11) |];
    [| Tile (Some Farm, 11); Tile (Some Farm, 10); Tile (Some Farm, 12); Tile (None, 10) |];
  |],
  Forest )
in
let mock_chunk2 =
  Chunk
    ( [|
        [| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];
        [| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];
        [| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];
        [| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |];
      |],
      Forest )
in
let mock_chunk3 =
  Chunk
    ( [|
        [| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];
        [| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];
        [| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];
        [| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |];
      |],
      Forest )
in
let mock_chunk4 =
  Chunk
    ( [|
        [| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];
        [| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];
        [| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];
        [| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |];
      |],
      Forest )
in
let mock_chunk5 =
  Chunk
    ( [|
        [| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];
        [| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];
        [| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];
        [| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |];
      |],
      Forest )
in
let mock_chunk6 =
  Chunk
    ( [|
        [| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];
        [| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];
        [| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];
        [| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |];
      |],
      Forest )
in
let mock_chunk7 =
  Chunk
    ( [|
        [| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];
        [| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];
        [| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];
        [| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |];
      |],
      Forest )
in
let mock_chunk8 =
  Chunk
    ( [|
        [| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];
        [| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];
        [| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];
        [| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |];
      |],
      Forest )
in
let mock_chunk9 =
  Chunk
    ( [|
        [| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];
        [| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];
        [| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];
        [| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |];
      |],
      Forest )
in
let mock_map =
  [|[|mock_chunk2;mock_chunk7;mock_chunk8|]; 
    [|mock_chunk3;mock_chunk1;mock_chunk5|];
    [|mock_chunk4;mock_chunk6;mock_chunk9|]|]
in
r_buildout Farm mock_map [(1,1)] ;
let a = Yojson.to_file "map1.json" (serialize_map  mock_map)
in ()
;;
assert ( mock_map <>( [|
  [|mock_chunk2;mock_chunk2;mock_chunk2|]; 
  [|mock_chunk2;mock_chunk1;mock_chunk2|];
  [|mock_chunk2;mock_chunk2;mock_chunk2|]|])
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


let mock_chunk1 = Chunk
( [|
    [| Tile (Some Farm, 10); Tile (Some Farm, 11); Tile (Some Farm, 12); Tile (Some Farm, 13) |];
    [| Tile (Some Farm, 9); Tile (Some Farm, 10); Tile (Some Farm, 11); Tile (Some Farm, 12) |];
    [| Tile (Some Farm, 10); Tile (Some Farm, 10); Tile (Some Farm, 11); Tile (Some Farm, 11) |];
    [| Tile (Some Farm, 11); Tile (Some Farm, 10); Tile (Some Farm, 12); Tile (None, 10) |];
  |],
  Forest )
in
let mock_chunk2 =
  Chunk
    ( [|
        [| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];
        [| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];
        [| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];
        [| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |];
      |],
      Forest )
in
let mock_chunk3 =
  Chunk
    ( [|
        [| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];
        [| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];
        [| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];
        [| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |];
      |],
      Forest )
in
let mock_chunk4 =
  Chunk
    ( [|
        [| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];
        [| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];
        [| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];
        [| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |];
      |],
      Forest )
in
let mock_chunk5 =
  Chunk
    ( [|
        [| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];
        [| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];
        [| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];
        [| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |];
      |],
      Forest )
in
let mock_chunk6 =
  Chunk
    ( [|
        [| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];
        [| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];
        [| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];
        [| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |];
      |],
      Forest )
in
let mock_chunk7 =
  Chunk
    ( [|
        [| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];
        [| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];
        [| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];
        [| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |];
      |],
      Forest )
in
let mock_chunk8 =
  Chunk
    ( [|
        [| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];
        [| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];
        [| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];
        [| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |];
      |],
      Forest )
in
let mock_chunk9 =
  Chunk
    ( [|
        [| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];
        [| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];
        [| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];
        [| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |];
      |],
      Forest )
in
let mock_map =
  [|[|mock_chunk2;mock_chunk7;mock_chunk8|]; 
    [|mock_chunk3;mock_chunk1;mock_chunk5|];
    [|mock_chunk4;mock_chunk6;mock_chunk9|]|]
in
r_buildin Farm mock_map [(1,1)] ;
let a = Yojson.to_file "map1.json" (serialize_map  mock_map)
in ()
;;
assert ( mock_map <>( [|
  [|mock_chunk2;mock_chunk2;mock_chunk2|]; 
  [|mock_chunk2;mock_chunk1;mock_chunk2|];
  [|mock_chunk2;mock_chunk2;mock_chunk2|]|])
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


let mock_chunk1 = Chunk
( [|
    [| Tile (Some Farm, 10); Tile (Some Farm, 11); Tile (Some Farm, 12); Tile (Some Farm, 13) |];
    [| Tile (Some Farm, 9); Tile (Some Farm, 10); Tile (Some Farm, 11); Tile (Some Farm, 12) |];
    [| Tile (Some Farm, 10); Tile (Some Farm, 10); Tile (Some Farm, 11); Tile (Some Farm, 11) |];
    [| Tile (Some Farm, 11); Tile (Some Farm, 10); Tile (Some Farm, 12); Tile (None, 10) |];
  |],
  Forest )
in
let mock_chunk2 =
  Chunk
    ( [|
        [| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];
        [| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];
        [| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];
        [| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |];
      |],
      Forest )
in

let mock_map =
([|[|Chunk( [|[| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];[| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];[| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];[| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |]|],  Desert );
Chunk( [|[| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];[| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];[| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];[| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |];|],Forest );
Chunk( [|[| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];[| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];[| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];[| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |];|],Forest )|]; 
[|Chunk( [|[| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];[| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];[| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];[| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |];|],Forest );
Chunk( [|    [| Tile (None, 10); Tile (None, 11); Tile (Some Farm, 12); Tile (Some Farm, 13) |];    [| Tile (Some Farm, 9); Tile (Some Farm, 10); Tile (Some Farm, 11); Tile (Some Farm, 12) |];    [| Tile (Some Farm, 10); Tile (Some Farm, 10); Tile (Some Farm, 11); Tile (Some Farm, 11) |];    [| Tile (Some Farm, 11); Tile (Some Farm, 10); Tile (Some Farm, 12); Tile (None, 10) |];  |],  Forest );
Chunk( [|[| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];[| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];[| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];[| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |];|],Forest )|];
[|Chunk( [|[| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];[| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];[| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];[| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |];|],Forest );
Chunk( [|[| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];[| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];[| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];[| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |];|],Forest );
Chunk( [|[| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];[| Tile (None, 9); Tile (None, 10); Tile (None, 11); Tile (None, 12) |];[| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];[| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |];|],Desert )|]|])
in
to_do (OutCity, Farm, Random) mock_map [(1,1)]; 
to_do (OutCity, Farm, Pref(Desert)) mock_map [(1,1)]; 
to_do (InCity, Farm, Random) mock_map [(1,1)]; 
to_do (InCity, Farm, Pref(Desert)) mock_map [(1,1)]; 
let a = Yojson.to_file "map1.json" (serialize_map  mock_map)
in a

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
