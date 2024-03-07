Config ExtraGrammar = "AutoLifter";
Config SampleIntMin = -3;
Config SampleIntMax = 3;
Config NonLinear = true;

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

msp =
  let mpp = fix (lambda f: Int -> List -> Int. \pre: Int. lambda l: List.
    match l with
      nil _ -> pre
    | cons {h, t} ->
      max pre (f (* h pre) t)
    end
  ) 1 in
  fix (\f: List -> Int. \xs: List.
  match xs with
    nil _ -> 1
  | cons {h, t} -> max (mpp xs) (f t)
  end
);

main = single_pass msp;