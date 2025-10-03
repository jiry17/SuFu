Config ExtraGrammar = "AutoLifter";
Config SampleIntMin = 0;
Config SampleIntMax = 1;
Config SampleSize = 20;

Inductive List = nil Unit | cons {Int, List};

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

zafter1 = fix (
  \f: Bool -> List -> Bool. \seen1: Bool. \xs: List.
  match xs with
    nil _ -> false
  | cons {h, t} ->
    if and seen1 (== h 0) then true
    else f (or seen1 (== h 1)) t
  end
) false;

main = single_pass zafter1;