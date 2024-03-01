/*Library for lists*/

Inductive List = cons {Int, List} | nil Unit;

fold = \oplus: Int->Int->Int. \e: Int.
  fix (lambda f: List->Int. lambda x: List.
    match x with
      cons {h, t} -> oplus h (f t)
    | nil _ -> e
    end
  );

length = fold (\x: Int. \y: Int. + y 1) 0;
sum = fold (\x: Int. \y: Int. + x y) 0;

head = lambda l: List. lambda default: Int.
  match l with
    cons {h, t} -> h
  | nil _ -> default
  end;

concat = fix (lambda f: List->List->List. lambda x: List. lambda y: List.
  match x with
    cons {h, t} -> cons {h, f t y}
  | nil _ -> y
  end
);

split = fix (lambda f: List->Int->{List, List}. lambda x: List. lambda n: Int.
  if (< n 1) then {nil unit, x}
  else match x with
    cons {h, t} -> let res = f t (- n 1) in {cons {h, res.1}, res.2}
  | _ -> {nil unit, nil unit}
  end
);

/*Sketches for DAC*/

@Input oplus: Int->Int->Int;
@Input e: Int;

map = fix (lambda f: List->Compress List. lambda x: List.
  match x with
    cons {h, t} -> cons {h, f t}
  | nil _ -> nil unit
  end
);

@Start run = \x: List. fold oplus e (map x);