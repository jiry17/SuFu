Config ExtraGrammar = "AutoLifter";
Config SampleIntMin = 0;
Config SampleIntMax = 3;

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

inf = 100;

/*User provided programs*/

max = \a: Int. \b: Int. if < a b then b else a;

largest_peak = fix (
  \f: Int -> List -> Int. \cmo: Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} ->
    let cmo = if > h 0 then + cmo h else 0 in
    max cmo (f cmo t)
  end
) 0;

/*pos_prefix = fix (
  \f: List -> List. \xs: List.
  match xs with
    nil _ -> nil unit
  | cons {h, t} -> if > h 0 then cons {h, f t} else nil unit
  end
);*/

main = dac largest_peak;