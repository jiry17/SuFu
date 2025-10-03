Inductive Tree = leaf Int | node {Int, Tree, Tree};

is_sym_pair = fix (
  \f: Tree -> Tree -> Bool. \l: Tree. \r: Tree.
  match {l, r} with
    {leaf x1, leaf x2} -> == x1 x2
  | {node {x1, l1, r1}, node {x2, l2, r2}} -> and (== x1 x2) (and (f r1 l2) (f l1 r1))
  | _ -> false
  end
);

is_sym = fix (
  \f: Tree -> Bool. \t: Tree.
  match t with
    leaf _ -> true
  | node {_, l, r} -> and (is_sym_pair l r) (and (f l) (f r))
  end
);

max = \a: Int. \b: Int. if < a b then b else a;

spec = fix (
  \f: Tree -> Int. \t: Tree.
  match t with
    leaf w -> 0
  | node {w, l, r} -> + 1 (+ (f l) (f r))
  end
);

target = fix (
  \f: Tree -> Compress Tree. \t: Tree.
  match t with
    leaf w -> leaf w
  | node {w, l, r} -> node {w, f l, r} /*Avoid the recursion of r*/
  end
);

/* Customized Generator */

Inductive List = elt Int | cons {Int, List};

depth_lim = 4;
gen = (fix (
  \f: Int -> List -> Tree. \depth: Int. \xs: List.
  match xs with
    elt w -> leaf w
  | cons {h, t} ->
    if == 0 depth then leaf h else
      let rem = - depth 1 in
        node {h, f rem t, f rem t}
  end
)) depth_lim;

main = \xs: List. let t = gen xs in
  if is_sym t then spec (target t) else 0;