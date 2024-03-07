Inductive BTree = empty Unit | node {Int, BTree, BTree};
Inductive Zipper = top Unit | left {Int, BTree, Zipper} | right {Int, BTree, Zipper};

max = \a: Int. \b: Int. if (< a b) then b else a;

mpath = fix (
  \f: BTree -> Int. \t: BTree.
  match t with
    empty _ -> 0
  | node {a, l, r} -> + a (max (f l) (f r))
  end
);

repr = fix (
  \f: Zipper -> Compress BTree. \z: Zipper.
  match z with
    top _ -> empty unit
  | left {w, tree, zz} ->
    /* (mpath tree) is given in Synduce's template*/
    let tw = mpath tree in
      node {w, tree, f zz}
  | right {w, tree, zz} ->
    let tw = mpath tree in
      node {w, f zz, tree}
  end
);

main = \z: Zipper. mpath (repr z);