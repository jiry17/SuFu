Inductive List = cons {Int, List} | nil Unit;

append = fix (
  \f: List -> Int -> List. \x: List. \w: Int.
  match x with
    cons {h, t} -> cons {h, f t w}
  | nil _ -> cons {w, nil unit}
  end 
);

sum = fix (
  \f: List -> Int. \x: List.
  match x with 
    cons {h, t} -> + h (f t)
  | nil _ -> 0
  end
);

@Input sth: Int;

rev = fix (\rev: List -> Compress List. \x: List.
  match x with 
    cons {h, t} -> append (rev t) h
  | nil _ -> nil unit
  end
);

@Start main = \x: List. sum (rev x);