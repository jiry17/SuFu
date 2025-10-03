Config ExtraGrammar = "AutoLifter";

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

max = fix (\f: List -> Int. \xs: List.
  match xs with
    nil _ -> - 0 inf
  | cons {h, t} ->
    let res = f t in if < res h then h else res
  end
);

main = single_pass max;