Config SampleIntMin = 0;
Config SampleSize = 6;

Inductive List = nil Unit | cons {Int, List};
Inductive Tree = leaf Int | node {Tree, Tree};
Plan = Compress Tree;
Inductive PlanList = pnil Unit | pcons {Plan, PlanList};

snoc = \w: Int. fix (
  \f: List -> List. \xs: List.
  match xs with
    nil _ -> cons {w, nil unit}
  | cons {h, t} -> cons {h, f t}
  end
);

merge = fix (
  \f: PlanList -> PlanList -> PlanList. \xs: PlanList.
  \ys: PlanList.
  match xs with
    pnil _ -> ys
  | pcons {h, t} -> pcons {h, f t ys}
  end
);

combine =
let combine_single = \l: Plan. fix (
  \f: PlanList -> PlanList. \xs: PlanList.
  match xs with
    pnil _ -> xs
  | pcons {h, t} -> pcons {node {l, h}, f t}
  end
) in fix (
  \f: PlanList -> PlanList -> PlanList. \xs: PlanList. \ys: PlanList.
  match xs with
    pnil _ -> xs
  | pcons {h, t} -> merge (combine_single h ys) (f t ys)
  end
);

/*Generate all plans*/
generate = fix (
  \f: List -> PlanList. \xs: List.
  match xs with
    nil _ -> pnil unit
  | cons {h, nil _} -> pcons {leaf h, pnil unit}
  | cons {h, t} -> fix (
    \g: List -> List -> PlanList. \l: List. \r: List.
    let res = combine (f l) (f r) in
    match r with
      cons {_, nil _} -> res
    | cons {rh, rt} -> merge res (g (snoc rh l) rt)
    end
    ) (cons {h, nil unit}) t
  end
);

/*Get best plan*/
eval = \xs: Tree. (fix (
  \f: Tree -> {Int, Int}. \t: Tree.
  match t with
    leaf w -> {0, w}
  | node {l, r} ->
    let lres = f l in let rres = f r in
      {+ (+ lres.1 lres.2) (+ rres.1 rres.2), + lres.2 rres.2}
  end
) xs).1;

min = \a: Int. \b: Int. if < a b then a else b;

get_best = fix (
  \f: PlanList -> Int. \xs: PlanList.
  match xs with
    pnil _ -> 1000
  | pcons {h, t} -> min (eval h) (f t)
  end
);

/*Main program*/
main = \xs: List. get_best (generate xs);

/*xs1 = cons {1, cons {2, cons {3, nil unit}}};
generate xs1;
main xs1;
xs2 = cons {1, cons {3, cons {2, nil unit}}};
main xs2;
xs3 = cons {13, cons {7, cons {8, cons {16, cons {21, cons {4, cons {18, nil unit}}}}}}};
main xs3;
main 1;*/
