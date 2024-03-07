Inductive List = elt Int | cons {Int, List};
Inductive MList = ielt Int | icons {Int, Int, MList};

repr = fix (
  \f: MList -> List. \m: MList.
  match m with
    ielt a -> elt a
  | icons {h, _, t} -> cons {h, f t}
  end
);

sum = fix (
  \f: List -> Int. \xs: List.
  match xs with
    elt a -> a
  | cons {h, t} -> + h (f t)
  end
);

is_memo = fix (
  \f: MList -> Bool. \m: MList.
  match m with
    ielt _ -> true
  | icons {h, s, t} -> and (== s (sum (repr m))) (f t)
  end
);

max = \a: Int. \b: Int. if < a b then b else a;

spec = fix (
  \f: List -> Int. \xs: List.
  match xs with
    elt x -> x
  | cons {h, t} -> max (f t) (sum xs)
  end
);

target = fix (
  \f: MList -> Compress MList. \m: MList.
  match m with
    ielt x -> m
  | icons {h, s, t} -> icons {h, s, f t}
  end
);

/* Customized generator, not used
gen = fix (
  \f: Tree -> TreeMemo. \t: Tree.
  match t with
    leaf a -> mleaf {if < a 2 then 1 else 0, a}
  | node {a, l, r} ->
    let res = {f l, f r} in
      mnode {+ (if < a 2 then 1 else 0) (+ (memo res.1) (memo res.2)), a, res.1, res.2}
  end
);*/

main = /*\t: Tree. let mt = gen t in*/
  \m: MList.
    if is_memo m then spec (repr (target m)) else 0;