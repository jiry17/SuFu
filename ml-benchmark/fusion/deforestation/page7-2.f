Config NonLinear = true;

Inductive Tree = leaf Int | branch {Tree, Tree};

square = \x: Int. * x x;

squaretr = fix (
  \f: Tree -> Compress Tree. \t: Tree.
  match t with
    leaf w -> leaf (square w)
  | branch {l, r} -> branch {f l, f r}
  end
);

sumtr = fix (
  \f: Tree -> Int. \t: Tree.
  match t with
    leaf w -> w
  | branch {l, r} -> + (f l) (f r)
  end
);

main = \t: Tree. sumtr (squaretr t);