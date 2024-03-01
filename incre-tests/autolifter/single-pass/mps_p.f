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

max = \a: Int. \b: Int. if < a b then b else a;

mps_p = \xs: List. (fix (
  \f: List -> Int -> Int -> Int -> Int -> Int. \xs: List.
  \i: Int. \mps: Int. \pos: Int. \sum: Int.
  match xs with
    nil _ -> pos
  | cons {h, t} ->
    let sum = + sum h in
    let pos = if > sum mps then i else pos in
    let mps = max mps sum in
      f t (+ i 1) mps pos sum
  end
) xs 0 0 0 0);

main = single_pass mps_p;