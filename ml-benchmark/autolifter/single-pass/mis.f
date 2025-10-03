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

mis = fix (
  \f: Int -> Int -> List -> Int. \p: Int. \np: Int. \xs: List.
  match xs with
    nil _ -> max p np
  | cons {h, t} ->
    f (+ h np) (max p np) t
  end
) 0 0;

main = single_pass mis;