Inductive Tree = leaf Int | node {Int, Tree, Tree};
Inductive TreeMemo = mleaf Int | mnode {Int, Int, TreeMemo, TreeMemo};

memo = \t: TreeMemo.
  match t with
    mleaf _ -> 1
  | mnode {x, _, _, _} -> x
  end;

is_memo = fix (
  \f: TreeMemo -> Bool. \t: TreeMemo.
  match t with
    mleaf x -> true
  | mnode {n, a, l, r} -> and (== n (+ 1 (+ (memo l) (memo r)))) (and (f l) (f r))
  end
);

repr = fix (
  \f: TreeMemo -> Tree. \t: TreeMemo.
  match t with
    mleaf a -> leaf a
  | mnode {n, a, l, r} -> node {a, f l, f r}
  end
);

target = fix (
  \f: TreeMemo -> Compress TreeMemo. \t: TreeMemo.
  match t with
    mleaf a -> mleaf a
  | mnode {n, a, l, r} -> mnode {n, a, l, r} /*Avoid recursions of l and r*/
  end
);

spec = fix (
  \f: Tree -> Int. \t: Tree.
  match t with
    leaf a -> 1
  | node {a, l, r} -> + 1 (+ (f l) (f r))
  end
);

/* Customized generator, not used */
gen = fix (
  \f: Tree -> TreeMemo. \t: Tree.
  match t with
    leaf a -> mleaf a
  | node {a, l, r} ->
    let res = {f l, f r} in
      mnode {+ 1 (+ (memo res.1) (memo res.2)), a, res.1, res.2}
  end
);

main =
  \mt: TreeMemo.
    if is_memo mt then spec (repr (target mt)) else 0;