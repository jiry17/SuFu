Config ExtraGrammar = "AutoLifter";

Inductive List = nil Unit | cons {Int, List};

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

mts_p = \xs: List. (fix (
  \f: List -> Int -> Int -> Int -> Int. \xs: List.
  \i: Int. \pos: Int. \mts: Int.
  match xs with
    nil _ -> pos
  | cons {h, t} ->
    let pos = if < (+ mts h) 0 then i else pos in
    let mts = max 0 (+ mts h) in
      f t (+ i 1) pos mts
  end
) xs 0 -1 0);

main = single_pass mts_p;