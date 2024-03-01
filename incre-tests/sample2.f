Inductive List = cons {Int, List} | nil Unit;

pushback = fix (
  \f: List->Int->List. \l: List. \v: Int.
  match l with 
    cons {h, t} -> cons {h, f t v}
  | nil _ -> cons {v, nil unit}
  end
);

sum = fix (
  \f: List->Int. \l: List.
  match l with
    cons {h, t} -> + h (f t)
  | nil _ -> 0
  end
);

func = fix (
  \f: List->Compress List. \l: List.
  match l with 
    nil _ -> nil unit
  | cons {h, t} ->
    if (< h 0) then f t 
    else (pushback (f t ) h)
  end
);

run = lambda l: List. sum (func l);