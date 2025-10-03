Config ExtraGrammar = "AutoLifter";
Config NonLinear = true;
Config SampleIntMin = 0;
Config SampleIntMax = 9;
Config SampleSize = 5;

Inductive List = cons {Int, List} | nil Unit;

base = 10;

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

atoi = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> (+ (* base (f t)) h)
  end
);

main = single_pass atoi;