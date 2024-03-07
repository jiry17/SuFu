Inductive Tree = elt Int | node {Int, Tree, Tree};

mod2 = \x: Int. - x (* (/ x 2) 2);

is_even = fix (
  \f: Tree -> Bool. \t: Tree.
  match t with
    elt a -> == (mod2 a) 0
  | node {a, l, r} -> and (and (== (mod2 a) 0) (f l)) (f r)
  end
);

spec = fix (
  \f: Tree -> Int. \t: Tree.
  match t with
    elt a -> mod2 a
  | node {a, l, r} -> + (mod2 a) (+ (f l) (f r))
  end
);

target = fix (
  \f: Tree -> Compress Tree. \t: Tree.
  match t with
    elt a -> elt a
  | node {a, l, r} -> node {a, l, r} /*Avoid recursions*/
  end
);

main = \t: Tree. if is_even t then spec (target t) else 0;