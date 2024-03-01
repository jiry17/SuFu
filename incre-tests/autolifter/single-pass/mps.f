Config ExtraGrammar = "AutoLifter";

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

/*User provided programs*/

mps = fix (lambda f: List -> Int. lambda l: List.
  match l with
    nil _ -> 0
  | cons {h, t} ->
    let res = (f t) in
      if < 0 (+ res h) then 0 else + res h
  end
);

main = single_pass mps;