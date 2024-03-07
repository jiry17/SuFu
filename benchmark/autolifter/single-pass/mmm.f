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

mmm = fix (
  \f: Int -> Int -> Int -> List -> Int. \p: Int. \np: Int. \z: Int. \xs: List.
  match xs with
    nil _ -> max p (max np z)
  | cons {h, t} ->
    f (+ (max z np) h) (- (max z p) h) (max p np) t
  end
) 0 0 0;

expected = fix (
  \f: Int -> Int -> Int -> List -> {Int, Int, Int}. \p: Int. \np: Int. \z: Int. \xs: List.
  match xs with
    nil _ -> {p, np, z}
  | cons {h, t} ->
    f (+ (max z np) h) (- (max z p) h) (max p np) t
  end
) 0 0 0;

main = single_pass mmm;