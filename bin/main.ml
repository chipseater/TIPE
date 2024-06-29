open Yojson.Safe
open Mapgen
open Dumpmap
open Village
open Decision

(* let map = gen_map 400 8 10 2;;

map |> serialize_map |> to_file "map1.json"

let village_exp =let a =
  (1, Vide, (stock_exp, needed_exp), (1, 1), [ (1, 1); (0, 0) ]) in 
  destroy_build (stock_exp, needed_exp)
    [ (1, 1); (0, 0) ]
    map map
  |> serialize_map |> to_file "map2.json" *)

  let stock_exp : data =
    [ (Bed, 0); (Food, 0); (People, 0); (Stone, 0); (Wood, 0) ]
  
  let needed_exp : data =
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
  assert (sum_data stock_exp needed_exp = stock_exp )

                          
