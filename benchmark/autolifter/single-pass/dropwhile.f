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

dropwhile = fix (
  \f: Int -> List -> Int. \i: Int. \xs: List.
  match xs with
    nil _ -> i
  | cons {h, t} -> if > h 0 then i else f (+ 1 i) t
  end
) 0;

main = single_pass dropwhile;