Config ExtraGrammar = "AutoLifter";
Config SampleIntMin = 1;
Config SampleIntMax = 3;
Config SampleSize = 20;

Inductive List = cons {Int, List} | nil Unit;

inf = 100;
two = 2;
three = 3;

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

count1s2s3s = fix (
  \f: Bool -> Bool -> List -> Int. \s1: Bool. \s2: Bool. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} ->
    let upd = if and (== h 3) (or s1 s2) then 1 else 0 in
    let s2 = and (== h 2) (or s1 s2) in
    let s1 = == h 1 in
    + upd (f s1 s2 t)
  end
) false false;

/*test2s1 = fix (
  \f: List -> Bool. \xs: List.
  match xs with
    nil _ -> false
  | cons {h, t} ->
    if == h 2 then f t
    else if == h 1 then true
    else false
  end
);

test2s3 = fix (
  \f: List -> Bool. \xs: List.
  match xs with
    nil _ -> false
  | cons {h, t} ->
    if == h 2 then f t
    else if == h 3 then true
    else false
  end
);*/

main = dac count1s2s3s;