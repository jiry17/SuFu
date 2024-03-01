Inductive List = cons {Int, List} | nil Unit;
Inductive List2D = cons2D {List, List2D} | nil2D Unit;

sum = fix (
  \f: List->Int. \l: List.
  match l with 
    cons {h, t} -> + h (f t)
  | nil _ -> 0
  end
);

max = lambda x: Int. lambda y: Int.
  if (> x y) then x else y;

maximum = fix (
  \f: List->Int. \l: List.
  match l with 
    cons {h, t} -> let res = f t in max h res
  | _ -> 0
  end
);

pushback = fix (
  \f: List->Int->List. \l: List. \a: Int.
  match l with
    cons {h, t} -> cons {h, f t a}
  | _ -> cons {a, nil unit}
  end
);

inits = (fix (
  \f: Compress List-> List->Compress List2D. \init: Compress List. \l: List.
  match l with
    cons {h, t} -> cons2D {init, f (pushback init h) t}
  | nil _ -> cons2D {init, nil2D unit}
  end
)) (nil unit);

maxsum = fix (
  \f: List2D->Int. \l: List2D.
  match l with 
    cons2D {h, t} -> max (sum h) (f t)
  | nil2D _ -> 0
  end
);

mps = lambda x: List. maxsum (inits x);

/*l = cons {2, cons {-4, cons {3, nil unit}}};
mps l;*/