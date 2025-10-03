Config ExtraGrammar = "AutoLifter";

Inductive List = cons {Int, List} | nil Unit;

single_pass = \v: List -> Bool.
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

line_sight = fix (
  \f: Int -> List -> Bool. \ma: Int. \xs: List.
  match xs with
    nil _ -> true
  | cons {h, nil _} -> >= h ma
  | cons {h, t} ->
    if > h ma then f h t else f ma t
  end
) (- 0 inf);

main = single_pass line_sight;