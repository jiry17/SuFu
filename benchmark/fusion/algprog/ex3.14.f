Inductive Tree = tip Int | bin {Tree, Tree};
Inductive PTree = ptip {Int, Int} | pbin {PTree, PTree};

tri = fix (
  \f: Tree -> Compress PTree. \t: Tree.
  match t with
    tip w -> ptip {0, w}
  | bin {l, r} ->
    let step = fix (
      \g: PTree -> PTree. \ys: PTree.
      match ys with
        ptip {n, w} -> ptip {+ n 1, w}
      | pbin {l, r} -> pbin {g l, g r}
      end
    ) in pbin {step (f l), step (f r)}
  end
);

tsum = fix (
  \f: PTree -> Int. \t: PTree.
  match t with
    ptip {n, w} -> * n w
  | pbin {l, r} -> + (f l) (f r)
  end
);

main = \t: Tree. tsum (tri t);
