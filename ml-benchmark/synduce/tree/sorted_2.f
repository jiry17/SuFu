Inductive Tree = leaf Int | node {Int, Tree, Tree};

spec = fix (
  \f: Int -> Tree -> Bool. \y: Int. \t: Tree.
  match t with
    leaf a -> > y a
  | node {a, l, r} ->
    let r1 = (f a l) in
    let r2 = (f a r) in
    and r1 (and r2 (> y a))
  end
);

repr = fix (
  \f: {Int, Tree} -> Compress {Int, Tree}. \t: {Int, Tree}.
  match t with
    {pre, leaf a} -> {pre, leaf a}
  | {pre, node {a, l, r}} ->
    let lres = f {a, l} in
      let rres = f {a, r} in
        {pre, node {a, lres.2, rres.2}}
  end
);

main = \key: Int. \t: Tree.
  let res = repr {key, t} in
    spec key res.2;
