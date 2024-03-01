Inductive Tree = tip Int | bin {Tree, Tree};

tri = \op: Int -> Int. fix (
  \f: Tree -> Compress Tree. \t: Tree.
  match t with
    tip _ -> tip 0
  | bin {l, r} ->
    let step = fix (
      \g: Tree -> Tree. \ys: Tree.
      match ys with
        tip w -> tip (op w)
      | bin {l, r} -> bin {g l, g r}
      end
    ) in bin {step (f l), step (f r)}
  end
);

op = \x: Int. + 1 x;

max = \a: Int. \b: Int. if < a b then b else a;

maximum = fix (
  \f: Tree -> Int. \t: Tree.
  match t with
    tip w -> w
  | bin {l, r} -> max (f l) (f r)
  end
);

main = \t: Tree. maximum (tri op t);