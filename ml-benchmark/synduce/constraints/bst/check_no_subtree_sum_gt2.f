Inductive Tree = leaf Int | node {Int, Tree, Tree};
Inductive MTree = mleaf Int | mnode {Int, Int, MTree, MTree};

min = \a: Int. \b: Int. if < a b then a else b;
max = \a: Int. \b: Int. if < a b then b else a;

tmin = fix (
  \f: MTree -> Int. \t: MTree.
  match t with
    mleaf w -> w
  | mnode {w, _, l, r} -> min w (min (f l) (f r))
  end
);

tmax = fix (
  \f: MTree -> Int. \t: MTree.
  match t with
    mleaf w -> w
  | mnode {w, _, l, r} -> max w (max (f l) (f r))
  end
);

tsum = fix (
  \f: MTree -> Int. \t: MTree.
  match t with
    mleaf w -> w
  | mnode {w, _, l, r} -> + w (+ (f l) (f r))
  end
);

is_bst = fix (
  \f: MTree -> Bool. \t: MTree.
  match t with
    mleaf w -> > w 0
  | mnode {w, s, l, r} -> and (and (> w 0) (== s (+ (tsum l) (tsum r))))
                              (and (and (>= w (tmax l)) (<= w (tmin r))) (and (f l) (f r)))
  end
);

@Input lim: Int;

spec = \t: Tree. (fix (
  \f: Tree -> {Bool, Int}. \t: Tree.
  match t with
    leaf x -> {<= x lim, x}
  | node {a, l, r} ->
    let lres = f l in let rres = f r in
      let sum = + a (+ lres.2 rres.2) in
        {and (<= sum lim) (and lres.1 rres.1), sum}
  end
) t).1;

drop_tag = fix (
  \f: MTree -> Tree. \t: MTree.
  match t with
    mleaf x -> leaf x
  | mnode {a, s, l, r} -> node {a, f l, f r}
  end
);

/*Customized generator*/

add_tag = fix (
  \f: Tree -> MTree. \t: Tree.
  match t with
    leaf x -> mleaf x
  | node {a, l, r} ->
    let lres = f l in let rres = f r in
      let s = + (tsum lres) (tsum rres) in
        mnode {a, s, lres, rres}
  end
);

target = fix (
  \f: MTree -> Compress MTree. \t: MTree.
  match t with
    mleaf x -> mleaf x
  | mnode {a, s, l, r} ->
    if > a lim then mnode {a, s, l, r} /*Avoid recursions*/
    else mnode {a, s, f l, f r}
  end
);

main = \t: Tree.
  let inp = add_tag t in
    if is_bst inp then spec (drop_tag (target inp)) else false;