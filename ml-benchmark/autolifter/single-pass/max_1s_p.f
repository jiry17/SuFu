Config ExtraGrammar = "AutoLifter";
Config SampleIntMin = 0;
Config SampleIntMax = 1;
Config SampleSize = 20;

Inductive List = cons {Int, List} | nil Unit;

single_pass = \v: List -> {Int, Int}.
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

max1s_with_pos = fix (\f: Int -> Int -> List -> {Int, Int}. \pre: Int. \i: Int. \xs: List.
  match xs with
    nil _ ->
      let len = - i pre in
        {len, pre}
  | cons {h, t} ->
      if == h 1 then f pre (+ i 1) t
      else let len = - i pre in
        let res = f (+ i 1) (+ i 1) t in
          if >= len res.1 then {len, pre} else res
  end
) 0 0;

main = single_pass max1s_with_pos;