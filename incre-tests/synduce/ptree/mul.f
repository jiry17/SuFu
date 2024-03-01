Inductive Tree = leaf Unit | node {Int, Tree, Tree};
Inductive PTree = pleaf Unit | pnode {Int, PList}
     with PList = pnil Unit | pcons {PTree, PList};

repr = fix (
  \f: PTree -> Compress Tree. \pt: PTree.
  match pt with
    pleaf _ -> leaf unit
  | pnode {a, xs} ->
    let l2t = fix (
      \g: PList -> Compress Tree. \xs: PList.
      match xs with
        pnil _ -> leaf unit
      | pcons {h, t} -> node {0, f h, g t}
      end
    ) in
      node {a, leaf unit, l2t xs}
  end
);

spec = fix (
  \f: Tree -> Int. \t: Tree.
  match t with
    leaf _ -> 1
  | node {a, l, r} -> * a (* (f l) (f r))
  end
);

main = \x: PTree. spec (repr x);

/*l0 = pnil unit;
t1 = pnode {5, l0};
l1 = pcons {t1, pcons {t1, l0}};
t2 = pnode {3, l1};
repr t2;*/