Config ExtraGrammar = "AutoLifter";
Config SampleIntMin = 0;
Config SampleIntMax = 1;
Config SampleSize = 20;

Inductive List = cons {Int, List} | nil Unit;

is_even = \a: Int. == a (* 2 (/ a 2));

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

longest_odd10s = fix (
  \f: Bool -> Bool -> Int -> List -> Int. \s1: Bool. \s2: Bool.
  \len: Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} ->
    let s1 = and s2 (== h 1) in
    let s2 = == h 0 in
    let len = if s1 then + 1 len else if s2 then len else 0 in
    if is_even len then f s1 s2 len t
    else max len (f s1 s2 len t)
  end
) false false 0;

main = single_pass longest_odd10s;