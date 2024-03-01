Config ExtraGrammar = "AutoLifter";
Config SampleIntMin = 0;
Config SampleIntMax = 5;

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

count10s2 = fix (
  \f: Bool -> List -> Int. \s0: Bool. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} ->
    let upd = if and s0 (== h 2) then 1 else 0 in
    let s0 = or (== h 1) (and s0 (== h 0)) in
      + upd (f s0 t)
  end
) false;

/*first_non_zero = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> if == h 0 then f t else h
  end
);*/

main = single_pass count10s2;