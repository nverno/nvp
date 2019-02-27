fun exists predicate xs = 
  case xs of
      [] => false
    | x::xs' => predicate x orelse exists predicate xs'
