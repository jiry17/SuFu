Inductive Tree = leaf Int | node {Int, Tree, Tree};
Inductive TreeMemo = mleaf {Int, Int} | mnode {Int, Int, TreeMemo, TreeMemo};

memo = \t: TreeMemo.
  match t with
    mleaf {x, _} -> x
  | mnode {x, _, _, _} -> x
  end;

is_memo = fix (
  \f: TreeMemo -> Bool. \t: TreeMemo.
  match t with
    mleaf {n, x} -> and (>= n 0) (if < x 2 then == n 1 else == n 0)
  | mnode {n, a, l, r} ->
    let exp = + (if < a 2 then 1 else 0) (+ (memo l) (memo r)) in
      and (and (>= n 0) (== n exp)) (and (f l) (f r))
  end
);

repr = fix (
  \f: TreeMemo -> Tree. \t: TreeMemo.
  match t with
    mleaf {n, a} -> leaf a
  | mnode {n, a, l, r} -> node {a, f l, f r}
  end
);

spec = fix (
  \f: Tree -> Int. \t: Tree.
  match t with
    leaf a -> if < a 2 then 1 else 0
  | node {a, l, r} -> if < a 2 then + 1 (+ (f l) (f r)) else + (f l) (f r)
  end
);

target = fix (
  \f: TreeMemo -> Compress TreeMemo. \t: TreeMemo.
  match t with
    mleaf {n, a} -> if < a 2 then t else t
  | mnode {n, a, l, r} -> if < a 2 then t else t /*Avoid recursions*/
  end
);

/* Customized generator, not used */
gen = fix (
  \f: Tree -> TreeMemo. \t: Tree.
  match t with
    leaf a -> mleaf {if < a 2 then 1 else 0, a}
  | node {a, l, r} ->
    let res = {f l, f r} in
      mnode {+ (if < a 2 then 1 else 0) (+ (memo res.1) (memo res.2)), a, res.1, res.2}
  end
);

main = /*\t: Tree. let mt = gen t in*/
  \mt: TreeMemo.
    if is_memo mt then spec (repr (target mt)) else 0;