Config SampleIntMin = 0;

Inductive Tree = leaf Unit | node {Int, Tree, Tree};
Plan = Compress Tree;
Inductive PlanList = pnil Unit | pcons {Plan, PlanList};

is_valid_tree = \t: Tree.
  match t with
    leaf _ -> false
  | node {_, w, leaf _} -> true
  | _ -> false
  end;

merge = fix (
  \f: PlanList -> PlanList -> PlanList.
  \xs: PlanList. \ys: PlanList.
  match xs with
    pnil _ -> ys
  | pcons {h, t} -> pcons {h, f t ys}
  end
);

extend = \l: Plan. \r: Plan. \w: Int.
let is_used = fix (
  \f: Tree -> Bool. \p: Tree.
  match p with
    leaf _ -> false
  | node {w, _, r} ->
    if > w 0 then true else f r
  end
  ) l in
if is_used then
  pcons {node {0, l, r}, pnil unit}
else pcons {node {w, l, r}, pcons {node {0, l, r}, pnil unit}};

extend_all = \w: Int.
let extend_one = \l: Plan. fix (
  \f: PlanList -> PlanList -> PlanList.
  \xs: PlanList. \pre: PlanList.
  match xs with
    pnil _ -> pre
  | pcons {h, t} -> merge (extend l h w) (f t pre)
  end
) in fix (
  \f: PlanList -> PlanList -> PlanList.
  \xs: PlanList. \ys: PlanList.
  match xs with
    pnil _ -> pnil unit
  | pcons {h, t} -> extend_one h ys (f t ys)
  end
);

generate = fix (
  \f: Tree -> PlanList. \t: Tree.
  match t with
    leaf _ -> pcons {t, pnil unit}
  | node {w, l, r} ->
    let lres = f l in let rres = f r in
      extend_all w lres rres
  end
);

/*Select best plan*/
max = \a: Int. \b: Int. if < a b then b else a;

get_best = let eval = fix (
  \f: Tree -> Int. \t: Tree.
  match t with
    leaf _ -> 0
  | node {w, l, r} -> + w (+ (f l) (f r))
  end
) in fix (
  \f: PlanList -> Int. \xs: PlanList.
  match xs with
    pnil _ -> 0
  | pcons {h, t} -> max (eval h) (f t)
  end
);

/*Main program*/
main = \t: Tree. if is_valid_tree t then get_best (generate t) else 0;

/*t = node {10, node {4, leaf unit, node {4, leaf unit, leaf unit}}, leaf unit};
generate t;
main t;
main 1;*/