/*Library for lists*/

Inductive List = cons {Int, List} | nil Unit;

length = fix (lambda f: List->Int. lambda x: List.
  match x with
    cons {h, t} -> + (f t) 1
  | nil _ -> 0
  end
);

head = lambda l: List. lambda default: Int.
  match l with
    cons {h, t} -> h
  | nil _ -> default
  end;

sum = fix (lambda f: List->Int. lambda x: List.
  match x with
    cons {h, t} -> + (f t) h
  | nil _ -> 0
  end
);

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

dac = fix (lambda f: List -> Compress List. lambda x: List.
  let len = length x in
    if (< len 2) then x
    else let sp = split x (/ len 2) in
      concat (f sp.1) (f sp.2)
);

max = lambda x: Int. lambda y: Int. if (> x y) then x else y;

mps = fix (lambda f: List -> Int. lambda l: List.
  match l with
    nil _ -> 0
  | cons {h, t} ->
    let res = (f t) in
      max 0 (+ res h)
  end
);

compress_mps = lambda x: List. mps (dac x);