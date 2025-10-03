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

max_sum_between_ones = fix (
  \f: Int -> List -> Int. \cs: Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} ->
    let cs = if == h 1 then 0 else + cs h in
    max cs (f cs t)
  end
) 0;

/*prefix_till_one = fix (
  \f: List -> List. \xs: List.
  match xs with
    nil _ -> nil unit
  | cons {h, t} -> if == h 1 then nil unit else cons {h, f t}
  end
);*/

main = single_pass max_sum_between_ones;