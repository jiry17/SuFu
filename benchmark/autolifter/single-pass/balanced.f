Config ExtraGrammar = "AutoLifter";
Config SampleIntMin = -1;
Config SampleIntMax = 1;
Config SampleSize = 20;
Config ComposeNum = 2;

Inductive List = cons {Int, List} | nil Unit;

single_pass = \v: List -> Bool.
  let run = fix (
    \f: List -> Compress List. \xs: List.
    match xs with
      nil _ -> xs
    | cons {h, t} -> cons {h, f t}
    end
  ) in \xs: List.
  v (run xs);

/*User provided programs*/

balanced = fix (
  \f: Int -> List -> Bool. \cnt: Int. \xs: List.
  match xs with
    nil _ -> true
  | cons {h, t} ->
    let cnt = + cnt h in
      if < cnt 0 then false else f cnt t
  end
) 0;

main = single_pass balanced;