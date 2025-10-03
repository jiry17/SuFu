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

mas = fix (
  \f: Int -> Int -> List -> Int. \p: Int. \np: Int. \xs: List.
  match xs with
    nil _ -> max p np
  | cons {h, t} ->
    max (max p np) (f (+ (max np 0) h) (- (max p 0) h) t)
  end
) 0 0;

expected = \xs: List.
let res = (fix (
  \f: Int -> Int -> List -> {Int, Int}. \p: Int. \np: Int. \xs: List.
  match xs with
    nil _ -> {max p 0, max np 0}
  | cons {h, t} -> (f (+ (max np 0) h) (- (max p 0) h) t)
  end
) 0 0 xs) in
  max res.1 res.2;

main = single_pass mas;