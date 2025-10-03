Inductive Tree = telt Int | tnode {Int, Tree, Tree};
Inductive PTree = pelt Int | pnode {Int, List}
    with List = elt PTree | cons {PTree, List};

max = \a: Int. \b: Int. if < a b then b else a;

repr = fix (
  \f: Tree -> Compress PTree. \x: Tree.
  match x with
    telt a -> pelt a
  | tnode {a, l, r} -> 
    let repr_list = fix (
        \g: Tree -> Compress List. \y: Tree.
        match y with
          telt a -> let z = pelt a in elt z
        | tnode {a, l, r} -> cons {pelt a, cons {f l, g r}}
        end
    ) in
    pnode {a, cons {f l, repr_list r}}
  end
);

spec = fix (
  \f: PTree -> Int. \x: PTree.
  match x with
    pelt a -> a
  | pnode {a, l} -> 
    let maxh_aux = fix (
        \g: List -> Int. \y: List.
        match y with
          elt a -> f a
        | cons {hd, tl} -> f hd
        end
    ) in 
    max a (maxh_aux l)
  end
);

main = \x: Tree. spec (repr x);