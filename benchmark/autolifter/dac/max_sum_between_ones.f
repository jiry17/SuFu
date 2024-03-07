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

max_sum_between_ones = fix (
  \f: Int -> List -> Int. \cs: Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} ->
    let cs = if == h 1 then 0 else + cs h in
    max cs (f cs t)
  end
) 0;

/*prefix_till_one = fix (
  \f: List -> List. \xs: List.
  match xs with
    nil _ -> nil unit
  | cons {h, t} -> if == h 1 then nil unit else cons {h, f t}
  end
);*/

main = dac max_sum_between_ones;