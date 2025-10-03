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

main = single_pass trdmin;