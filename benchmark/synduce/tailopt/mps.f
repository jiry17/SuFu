Config ExtraGrammar = "AutoLifter";
Inductive List = nil Unit | cons {Int, List};

max = \x: Int. \y: Int. if > x y then x else y;

spec = \xs: List. (fix (
  \f: List -> {Int, Int}. \xs: List.
  match xs with
    nil _ -> {0,0}
  | cons {h, t} ->
    let r = f t in
    {+ h r.1, max 0 (+ h r.2)}
  end
) xs).2;

snoc = fix (
  \f: List -> Int -> List. \xs: List. \w: Int.
  match xs with
    nil _ -> cons {w, nil unit}
  | cons {h, t} -> cons {h, f t w}
  end
);

repr = (fix (
  \f: Compress List -> List -> Compress List.
  \pre: Compress List. \xs: List.
  match xs with
    nil _ -> pre
  | cons {h, t} -> f (snoc pre h) t
  end
)) (nil unit);

main = \xs: List. spec (repr xs);