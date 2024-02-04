type buildingType = MAISON | CHAMP | MINE | USINE | MOULIN
type material = LOGEMENT | GRAIN | MINERAIS | MACHINE | NOURITURE | RIEN

(* TODO: Reimplement using ocaml objects to levrage the power of inheritance *)

type building = {
  building_type: buildingType;
  (* The type of material produced by the building *)
  material: material;
  (* The possibly needed raw materials *)
  raw_material: material;
  (* The amount of material produced by the building *)
  production: int;
  (* The number of people to operate / maintain the building *)
  manpower: float;
  (* The amount of raw material to operate the building *)
  raw_material_amount: int;
  (* The level of mechanisation of the building *)
  mechanisation: int
}

let newBuilding building_type = match building_type with
  | MAISON -> {
      building_type = building_type;
      raw_material = NOURITURE;
      material = LOGEMENT;
      production = 5;
      manpower = 0.25; (* Only for maintenance *)
      raw_material_amount = 5;
      mechanisation = 1 (* Always 1 *)
    }
  | CHAMP -> {
      building_type = building_type;
      raw_material = RIEN;
      material = GRAIN;
      production = 5;
      manpower = 2;
      raw_material_amount = 0;
      mechanisation = 1
    }
  | MINE -> {
      building_type = building_type;
      raw_material = RIEN;
      material = MINERAIS;
      production = 5;
      manpower = 2;
      raw_material_amount = 0;
      mechanisation = 1
    }
  | USINE -> {
      building_type = building_type;
      raw_material = MINERAIS;
      material = MACHINE;
      production = 1;
      manpower = 5;
      raw_material_amount = 5;
      mechanisation = 1
    }
  | MOULIN -> {
      building_type = building_type;
      raw_material = GRAIN;
      material = MACHINE;
      production = 5;
      manpower = 2;
      raw_material_amount = 5;
      mechanisation = 1
    }
