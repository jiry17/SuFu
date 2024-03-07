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

mps_p = \xs: List. (fix (
  \f: List -> Int -> Int -> Int -> Int -> Int. \xs: List.
  \i: Int. \mps: Int. \pos: Int. \sum: Int.
  match xs with
    nil _ -> pos
  | cons {h, t} ->
    let sum = + sum h in
    let pos = if > sum mps then i else pos in
    let mps = max mps sum in
      f t (+ i 1) mps pos sum
  end
) xs 0 0 0 0);

main = dac mps_p;