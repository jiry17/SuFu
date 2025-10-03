Config ExtraGrammar = "AutoLifter";
Config SampleIntMin = 0;
Config SampleIntMax = 1;
Config SampleSize = 20;

Inductive List = cons {Int, List} | nil Unit;

single_pass = \v: List -> Int.
  let run = fix (
    \f: List -> Compress List. \xs: List.
    match xs with
      nil _ -> xs
    | cons {h, t} -> cons {h, f t}
    end
  ) in \xs: List.
  v (run xs);

inf = 100;

/*User provided programs*/

max = \a: Int. \b: Int. if < a b then b else a;

max1s = fix (\f: Int -> Int -> List -> Int. \ma: Int. \l: Int. \xs: List.
  match xs with
    nil _ -> max ma l
  | cons {h, t} ->
    if == h 0 then f (max ma l) 0 t else f ma (+ l 1) t
  end
) 0 0;

main = single_pass max1s;