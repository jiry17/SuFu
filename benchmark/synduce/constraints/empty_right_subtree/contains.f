Inductive Tree = nil Unit | leaf Int | node {Int, Tree, Tree};

size = fix (
  \f: Tree -> Int. \t: Tree.
  match t with
    nil _ -> 0
  | leaf _ -> 1
  | node {_, l, r} -> + 1 (+ (f l) (f r))
  end
);

empty_right = fix (
  \f: Tree -> Bool. \t: Tree.
  match t with
    node {_, l, r} -> and (== 0 (size r)) (f l)
  | _ -> true
  end
);

@Input w: Int;

spec = fix (
  \f: Tree -> Int. \t: Tree.
  match t with
    nil _ -> 0
  | leaf a -> if == a w then 1 else 0
  | node {a, l, r} ->
    if == a w then 1 else if == 1 (f l) then 1 else f r
  end
);

target = fix (
  \f: Tree -> Compress Tree. \t: Tree.
  match t with
    nil _ -> nil unit
  | leaf a -> leaf a
  | node {a, l, r} -> node {a, f l, r} /* Avoid the recursion on the right branch.*/
  end
);

main = \t: Tree. if empty_right t then spec (target t) else 0;