Config NonLinear = true;

Inductive List = nil Unit | cons {Int, List};
Inductive PList = pnil Unit | pcons {Int, Int, PList};

tri = fix (
  \f: List -> Compress PList. \xs: List.
  match xs with
    nil _ -> pnil unit
  | cons {h, t} ->
    let tail = (fix (
      \g: PList -> PList. \ys: PList.
      match ys with
        pnil _ -> pnil unit
      | pcons {n, h, t} -> pcons {+ n 1, h, g t}
      end
    )) (f t) in pcons {0, h, tail}
  end
);

tsum = fix (
  \f: PList -> Int. \xs: PList.
  match xs with
    pnil _ -> 0
  | pcons {n, h, t} -> + (* n h) (f t)
  end
);

main = \xs: List. tsum (tri xs);
