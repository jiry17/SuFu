Inductive BTree = empty Unit | node {Int, BTree, BTree};
Inductive Zipper = top Unit | left {Int, BTree, Zipper} | right {Int, BTree, Zipper};

max = \a: Int. \b: Int. if (< a b) then b else a;

spec = \t: BTree. (fix (
  \f: {Int, Int} -> BTree -> {Int, Int}. \s: {Int, Int}. \t: BTree.
  match t with
    empty _ -> s
  | node {a, l, r} ->
    let result = f s l in
    f {+ result.1 a, max result.2 (+ result.1 a)} r
  end
) {0,0} t).2;

repr = fix (
  \f: Zipper -> Compress BTree. \z: Zipper.
  match z with
    top _ -> empty unit
  | left {w, tree, zz} ->
      let info = spec tree in /*This invocation is provided in Synduce's template*/
        node {w, tree, f zz}
  | right {w, tree, zz} ->
      let info = spec tree in /*This invocation is provided in Synduce's template*/
        node {w, f zz, tree}
  end
);

main = \z: Zipper. spec (repr z);