Inductive Tree = leaf Int | node {Int, Tree, Tree};
Inductive TreeMemo = mleaf {Int, Int} | mnode {Int, Int, TreeMemo, TreeMemo};

memo = \t: TreeMemo.
  match t with
    mleaf {x, _} -> x
  | mnode {x, _, _, _} -> x
  end;

max = \a: Int. \b: Int. if < a b then b else a;

tmax = fix (
  \f: TreeMemo -> Int. \t: TreeMemo.
  match t with
    mleaf {_, x} -> x
  | mnode {_, x, l, r} -> max x (max (f l) (f r))
  end
);

is_memo = fix (
  \f: TreeMemo -> Bool. \t: TreeMemo.
  match t with
    mleaf {n, x} -> true
  | mnode {n, a, l, r} ->
    and (and (and (>= n (tmax l)) (>= n (tmax r))) (>= n a)) (and (f l) (f r))
  end
);

repr = fix (
  \f: TreeMemo -> Tree. \t: TreeMemo.
  match t with
    mleaf {n, a} -> leaf a
  | mnode {n, a, l, r} -> node {a, f l, f r}
  end
);

@Input key: Int;

spec = fix (
  \f: Tree -> Int. \t: Tree.
  match t with
    leaf a -> if > a key then a else 0
  | node {a, l, r} -> + (if > a key then a else 0) (+ (f l) (f r))
  end
);

target = fix (
  \f: TreeMemo -> Compress TreeMemo. \t: TreeMemo.
  match t with
    mleaf {n, a} -> t
  | mnode {n, a, l, r} ->
    if > key n then t /*Avoid recursions*/
    else mnode {n, a, f l, f r}
  end
);

main =
  \mt: TreeMemo.
    if is_memo mt then spec (repr (target mt)) else 0;