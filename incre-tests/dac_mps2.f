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
/*Sketches for DAC*/

dac = fix (lambda f: List -> Int -> {List, Compress List}. lambda x: List. lambda len: Int.
  if (== len 1) then
    match x with
      cons {h, t} -> {t, cons {h, nil unit}}
    end
  else let llen = (/ len 2) in
    let lres = f x llen in
      let rres = f lres.1 (- len llen) in
        {rres.1, concat lres.2 rres.2}
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

compress_mps = lambda x: List. mps (dac x (length x)).2 ;

/*
l = cons {2, cons {-1, cons {3, nil unit}}};
compress_mps l;*/