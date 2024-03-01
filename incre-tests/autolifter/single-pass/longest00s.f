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

longest00s = fix (
  \f: Int -> List -> Int.
  \len: Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} ->
    let len = if == h 0 then + len 1 else 0 in
    if is_even len then max len (f len t)
    else f len t
  end
) 0;

main = single_pass longest00s;