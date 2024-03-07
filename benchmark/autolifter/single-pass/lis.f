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

inf = 100;

/*User provided programs*/

max = \a: Int. \b: Int. if < a b then b else a;

max_sum_between_ones = fix (
  \f: Int -> Int -> List -> Int. \cs: Int. \pre: Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} ->
    let cs = if < pre h then + cs 1 else 0 in
    max cs (f cs h t)
  end
) 0 0;

/*inc_num = fix (
  \f: Int -> List -> Int. \pre: Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} ->
    if < pre h then + 1 (f h t) else 0
  end
);

dec_num = fix (
  \f: Int -> List -> Int. \pre: Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} ->
    if > pre h then + 1 (f h t) else 0
  end
);*/

main = single_pass max_sum_between_ones;