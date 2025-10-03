Config ExtraGrammar = "AutoLifter";
Config SampleIntMin = 1;
Config SampleIntMax = 3;

Inductive List = cons {Int, List} | nil Unit;

inf = 100;
two = 2;
three = 3;

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

count1s2s3s = fix (
  \f: Bool -> Bool -> List -> Int. \s1: Bool. \s2: Bool. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} ->
    let upd = if and (== h 3) (or s1 s2) then 1 else 0 in
    let s2 = and (== h 2) (or s1 s2) in
    let s1 = == h 1 in
    + upd (f s1 s2 t)
  end
) false false;

main = single_pass count1s2s3s;