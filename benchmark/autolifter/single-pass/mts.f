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

/*User provided programs*/

max = \a: Int. \b: Int. if < a b then b else a;

mts = \xs: List. (fix (\f: List -> {Int, Int}. \xs: List.
  match xs with
    nil _ -> {0, 0}
  | cons {h, t} ->
    let res = f t in
      {max (+ res.2 h) res.1, + res.2 h}
  end
) xs).1;

main = single_pass mts;