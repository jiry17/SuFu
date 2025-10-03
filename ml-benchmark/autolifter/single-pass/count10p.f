Config ExtraGrammar = "AutoLifter";
Config SampleIntMin = 0;
Config SampleIntMax = 1;
Config SampleSize = 20;

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

count10p = fix (
  \f: Bool -> Bool -> List -> Int. \s0: Bool. \s1: Bool. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} ->
    let upd = if and s1 (== h 1) then 1 else 0 in
    let s1 = and (== h 0) (or s0 s1) in
    let s0 = == h 1 in
      + upd (f s0 s1 t)
  end
) false false;

/*s1 = fix (
  \f: Bool -> Bool -> List -> Bool. \s0: Bool. \s1: Bool. \xs: List.
  match xs with
    nil _ -> s1
  | cons {h, t} -> f (== h 1) (and (== h 0) (or s0 s1)) t
  end
);*/

main = single_pass count10p;