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

inf = 100;

/*User provided programs*/

max = \a: Int. \b: Int. if < a b then b else a;

max_sum_between_ones = fix (
  \f: Int -> Int -> List -> Int. \cs: Int. \pre: Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} ->
    let cs = if < pre h then + cs 1 else 0 in
    max cs (f cs h t)
  end
) 0 0;

/*inc_num = fix (
  \f: Int -> List -> Int. \pre: Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} ->
    if < pre h then + 1 (f h t) else 0
  end
);

dec_num = fix (
  \f: Int -> List -> Int. \pre: Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} ->
    if > pre h then + 1 (f h t) else 0
  end
);*/

main = dac max_sum_between_ones;