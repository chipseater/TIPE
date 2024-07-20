let get_popuplation village =
  let _, _, (stock, _), _, _ = village in
  search stock People
