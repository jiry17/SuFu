Config ExtraGrammar = "AutoLifter";

Inductive List = cons {Int, List} | nil Unit;

length = fix (\f: List->Int. \x: List.
  match x with
    cons {h, t} -> + (f t) 1
  | nil _ -> 0
  end
);

concat = fix (\f: List->List->List. \x: List. \y: List.
  match x with
    cons {h, t} -> cons {h, f t y}
  | nil _ -> y
  end
);

split = \xs: List. fix (\f: List->Int->{List, List}. \x: List. \n: Int.
  if (< n 1) then {nil unit, x}
  else match x with
    cons {h, t} -> let res = f t (- n 1) in {cons {h, res.1}, res.2}
  | _ -> {nil unit, nil unit}
  end
) xs (/ (length xs) 2);

dac = \v: List -> Int. \xs: List.
  let run = fix (\f: List -> Compress List. \xs: List.
    match xs with
      nil _ -> xs
    | cons {_, nil _} -> xs
    | _ -> let sp = split xs in
      concat (f sp.1) (f sp.2)
    end
  ) in v (run xs);

/*User provided programs*/

max = \a: Int. \b: Int. if < a b then b else a;

mts_p = \xs: List. (fix (
  \f: List -> Int -> Int -> Int -> Int. \xs: List.
  \i: Int. \pos: Int. \mts: Int.
  match xs with
    nil _ -> pos
  | cons {h, t} ->
    let pos = if < (+ mts h) 0 then i else pos in
    let mts = max 0 (+ mts h) in
      f t (+ i 1) pos mts
  end
) xs 0 -1 0);

main = dac mts_p;