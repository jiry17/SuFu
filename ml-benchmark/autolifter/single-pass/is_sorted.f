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

is_sorted = fix (
  \f: Int -> List -> Bool. \pre: Int. \xs: List.
  match xs with
    nil _ -> true
  | cons {h, t} -> if >= pre h then false else f h t
  end
) (- 0 inf);

main = single_pass is_sorted;