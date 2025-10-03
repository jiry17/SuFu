Inductive Tree = leaf {Int, Int} | node {Int, Int, Tree, Tree};

min = \a: Int. \b: Int. if < a b then a else b;
max = \a: Int. \b: Int. if < a b then b else a;

tmin = fix (
  \f: Tree -> Int. \t: Tree.
  match t with
    leaf {a, b} -> a
  | node {a, b, l, r} -> min a (min (f l) (f r))
  end
);

tmax = fix (
  \f: Tree -> Int. \t: Tree.
  match t with
    leaf {a, b} -> a
  | node {a, b, l, r} -> max a (max (f l) (f r))
  end
);

is_bst = fix (
  \f: Tree -> Bool. \t: Tree.
  match t with
    leaf {a, b} -> < a b
  | node {a, b, l, r} -> and (< a b) (and (and (> a (tmax l)) (< a (tmin r))) (and (f l) (f r)))
  end
);

@Input lo: Int;
@Input hi: Int;

inside = \a: Int. \b: Int. and (< lo a) (< b hi);

spec = fix (
  \f: Tree -> Bool. \t: Tree.
  match t with
    leaf {a, b} -> inside a b
  | node {a, b, l, r} -> or (or (inside a b) (f l)) (f r)
  end
);

target = fix (
  \f: Tree -> Compress Tree. \t: Tree.
  match t with
    leaf {a, b} -> leaf {a, b}
  | node {a, b, l, r} ->
    if and (> b hi) (< a lo) then
      node {a, b, l, f r} /*Avoid the recursion on the left branch*/
    else node {a, b, f l, f r}
  end
);

main = \t: Tree. if is_bst t then spec (target t) else false;