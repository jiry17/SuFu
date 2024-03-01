Inductive Tree = leaf Int | node {Int, Tree, Tree};
Inductive List = elt Int | cons {Int, List};

min = \a: Int. \b: Int. if < a b then a else b;
max = \a: Int. \b: Int. if < a b then b else a;

tmin = fix (
  \f: Tree -> Int. \t: Tree.
  match t with
    leaf w -> w
  | node {w, l, r} -> min w (min (f l) (f r))
  end
);

tmax = fix (
  \f: Tree -> Int. \t: Tree.
  match t with
    leaf w -> w
  | node {w, l, r} -> max w (max (f l) (f r))
  end
);

is_bst = fix (
  \f: Tree -> Bool. \t: Tree.
  match t with
    leaf w -> true
  | node {w, l, r} -> and (and (>= w (tmax l)) (<= w (tmin r))) (and (f l) (f r))
  end
);

cat = fix (\f: List->List->List. \x: List. \y: List.
  match x with
    cons {h, t} -> cons {h, f t y}
  | elt w -> cons {w, y}
  end
);

repr = fix (
  \f: Tree -> List. \t: Tree.
  match t with
    leaf x -> elt x
  | node {x, l, r} -> cat (f l) (cons {x, f r})
  end
);

spec = fix (
  \f: List -> Int. \t: List.
  match t with
    elt x -> x
  | cons {h, t} -> max h (f t)
  end
);

target = fix (
  \f: Tree -> Compress Tree. \t: Tree.
  match t with
    leaf x -> leaf x
  | node {a, l, r} -> node {a, l, f r} /*avoid the recursion of l*/
  end
);

main = \t: Tree. if is_bst t then spec (repr (target t)) else 0;