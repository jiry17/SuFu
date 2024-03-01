Config ExtraGrammar = "AutoLifter";
Config SampleIntMin = 0;
Config SampleIntMax = 1;

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

inf = 100;

/*User provided programs*/

zsos = fix (
  \f: Bool -> List -> Bool. \an: Bool. \xs: List.
  match xs with
    nil _ -> true
  | cons {h, t} ->
    let an = and an (== h 1) in
      if or (== h 0) an then f an t else false
  end
) true;

main = single_pass zsos;