open Mapgen
open Dumpmap
open Yojson
open Mapmanage
;;
(* 
let map = gen_map 10 5 10 3;;
print_string (to_string (serialize_map map));; *)




open Village
open Decision

  let stock_exp : data =
    [ (Bed, 0); (Food, 0); (People, 0); (Stone, 0); (Wood, 0) ]

  let prod_exp : data =
    [ (Bed, 0); (Food, 100); (People, -25); (Stone, 0); (Wood, 0) ]

  let mock_chunk1 =
    Chunk
      ( [|
          [|Tile (None, 10); Tile (None, 11);Tile (Some House, 12); Tile (None, 13);|];
          [|Tile (None, 9);  Tile (None, 10);Tile (Some Quarry, 11);Tile (None, 12);|];
          [|Tile (None, 10); Tile (None, 10);Tile (Some Farm, 11);  Tile (None, 11);|];
          [|Tile (None, 11); Tile (None, 10);Tile (Some House, 12); Tile (None, 10);|];
        |],
        Forest )

  let mock_chunk2 =
    Chunk
      ( [|
          [| Tile (None, 10); Tile (None, 11); Tile (None, 12); Tile (None, 13) |];
          [| Tile (None, 9);  Tile (None, 10); Tile (None, 11); Tile (None, 12) |];
          [| Tile (None, 10); Tile (None, 10); Tile (None, 11); Tile (None, 11) |];
          [| Tile (None, 11); Tile (None, 10); Tile (None, 12); Tile (None, 10) |];
        |],
        Forest )

  let mock_chunk3 =
    Chunk
      ( [|
          [|Tile (None, 10);Tile (Some Farm, 11); Tile (None, 12);      Tile (None, 13);|];
          [|Tile (None, 9); Tile (Some Farm, 10); Tile (Some House, 11);Tile (None, 12);|];
          [|Tile (None, 10);Tile (Some Farm, 10); Tile (Some House, 11);Tile (None, 11);|];
          [|Tile (None, 11);Tile (Some Farm, 10); Tile (None, 12);      Tile (None, 10);|];
        |],
        Forest )

        let mock_map =  [|[|mock_chunk2;mock_chunk2|];
                          [|mock_chunk1;mock_chunk3|]|]


  ;;
  assert (sum_data stock_exp prod_exp = prod_exp );;
  assert (get_production_from_tile( (get_chunk_tiles mock_chunk1).(0).(1)) = stock_exp );;
  assert (get_production_from_tile( (get_chunk_tiles mock_chunk1).(0).(2)) = house_data_prodution );;
  let data_1 = sum_data house_data_prodution (sum_data house_data_prodution (sum_data quarry_data_prodution farm_data_prodution))
in
  assert (sum_chunk_production mock_chunk1 = data_1);;
  let data_1 = sum_data house_data_prodution (sum_data house_data_prodution (sum_data quarry_data_prodution farm_data_prodution))
in
let pos_li = [ (0,0) ; (1,0) ] in
  assert ( sum_chunk_list_production pos_li mock_map = data_1);;
  assert ( update_logistics (stock_exp, prod_exp) = (prod_exp,void_data));;

  let stock_exp1 : data =
    [ (Bed, 40); (Food, 4); (People, 35); (Stone, 0); (Wood, 0) ]

  let prod_exp1 : data =
    [ (Bed, 0); (Food, 10); (People, -25); (Stone, 0); (Wood, 0) ]

  let result = [(Bed, 0); (Food,10);(People, 14); (Stone, 0); (Wood,0)]
  ;;

 assert (update_all_logistics (stock_exp1,prod_exp1)= (result,void_data) );; 

 let pos_li = [ (0,0) ; (1,0) ] in
 let res_map = [|[|mock_chunk2;mock_chunk2|];[|mock_chunk2;mock_chunk3|]|]
in
(* print_string (to_string (serialize_map mock_map));print_string("\n\n\n"); *)
let paf = destroy_build (stock_exp,prod_exp) pos_li mock_map in
assert ( paf = (stock_exp,stock_exp));
(* print_string (to_string (serialize_map mock_map));; *)
assert(mock_map = res_map);;
