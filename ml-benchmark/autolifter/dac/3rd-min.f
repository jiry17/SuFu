Config ExtraGrammar = "AutoLifter";
Config ComposeNum = 2;
Config TermNum = 6;

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

min = \a: Int. \b: Int. if < a b then a else b;
max = \a: Int. \b: Int. if > a b then a else b;

trdmin = \xs: List. (fix (
  \f: List -> {Int, Int, Int}. \xs: List.
  match xs with
    nil _ -> {inf, inf, inf}
  | cons {h, t} ->
    let res = f t in
      {min res.1 (max res.2 h),
       min res.2 (max res.3 h),
       min res.3 h}
  end
) xs).1;

main = dac trdmin;