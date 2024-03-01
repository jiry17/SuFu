Config ExtraGrammar = "AutoLifter";

Inductive List = nil Unit | cons {Int, List};

sum = fix (
    \f: List -> Int. \xs: List. 
    match xs with
      nil _ -> 0
    | cons {h, t} -> + h (f t)
    end
);

spec = \xs: List. 
    sum xs;

repr = fix (
    \f: List -> Compress List. \xs: List. 
    match xs with
      nil _ -> nil unit
    | cons {h, t} -> cons {h, (f t)}
    end
);

main = \xs: List. spec(repr xs);