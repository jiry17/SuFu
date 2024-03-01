Config ExtraGrammar = "AutoLifter";
Config SampleIntMin = 0;
Config SampleIntMax = 3;

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

max = \a: Int. \b: Int. if < a b then b else a;

largest_peak = fix (
  \f: Int -> List -> Int. \cmo: Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} ->
    let cmo = if > h 0 then + cmo h else 0 in
    max cmo (f cmo t)
  end
) 0;

/*pos_prefix = fix (
  \f: List -> List. \xs: List.
  match xs with
    nil _ -> nil unit
  | cons {h, t} -> if > h 0 then cons {h, f t} else nil unit
  end
);*/

main = single_pass largest_peak;