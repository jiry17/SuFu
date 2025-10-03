Config ExtraGrammar = "AutoLifter";
Config SampleIntMin = 0;
Config SampleIntMax = 1;

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

cnt1s = fix (\f: Int -> Int -> List -> Int. \pre: Int. \cnt: Int. \xs: List.
  match xs with
    nil _ -> cnt
  | cons {h, t} ->
    let cnt = if and (== pre 0) (== h 1) then + cnt 1 else cnt in
      f h pre t
  end
) 0 0;

main = single_pass cnt1s;