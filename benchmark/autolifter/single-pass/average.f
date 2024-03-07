Config ExtraGrammar = "AutoLifter";

Inductive List = cons {Int, List} | nil Unit;

div = \a: Int. \b: Int.
  if == b 0 then 0 else / a b;

length = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> + 1 (f t)
  end
);

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

average = \xs: List.
  let len = length xs in
  let sum = fix (
    \f: List -> Int. \ys: List.
    match ys with
      nil _ -> 0
    | cons {h, t} -> + h (f t)
    end
  ) xs in div sum len;

main = single_pass average;