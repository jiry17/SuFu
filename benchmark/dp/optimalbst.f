Config NonLinear = true;
Config SampleIntMin = 0;
Config SampleSize = 6;

Inductive List = nil Unit | cons {Int, List};
Inductive Tree = leaf Unit | node {Int, Tree, Tree};
Plan = Compress Tree;
Inductive PlanList = pnil Unit | pcons {Plan, PlanList};

merge = fix (
  \f: PlanList -> PlanList -> PlanList.
  \xs: PlanList. \ys: PlanList.
  match xs with
    pnil _ -> ys
  | pcons {h, t} -> pcons {h, f t ys}
  end
);

combine = \w: Int.
let combine_single = \l: Plan. fix (
  \f: PlanList -> PlanList. \xs: PlanList.
  match xs with
    pnil _ -> xs
  | pcons {h, t} -> pcons {node {w, l, h}, f t}
  end
) in fix (
  \f: PlanList -> PlanList -> PlanList. \xs: PlanList. \ys: PlanList.
  match xs with
    pnil _ -> xs
  | pcons {h, t} -> merge (combine_single h ys) (f t ys)
  end
);

snoc = \w: Int. fix (
  \f: List -> List. \xs: List.
  match xs with
    nil _ -> cons {w, nil unit}
  | cons {h, t} -> cons {h, f t}
  end
);

/*Generate All*/
generate = fix (
  \f: List -> PlanList. \xs: List.
  match xs with
    nil _ -> pcons {leaf unit, pnil unit}
  | _ -> fix (
      \g: List -> List -> PlanList. \l: List. \r: List.
      match r with
        nil _ -> pnil unit
      | cons {root, newr} ->
        let newl = snoc root l in
          merge (combine root (f l) (f newr)) (g newl newr)
      end
    ) (nil unit) xs
  end
);

/*Get best*/
eval = \t: Tree. (fix (
  \f: Tree -> {Int, Int}. \t: Tree.
  match t with
    leaf _ -> {0, 0}
  | node {w, l, r} ->
    let lres = f l in let rres = f r in
    let sum = + w (+ lres.2 rres.2) in
      {+ sum (+ lres.1 rres.1), sum}
  end
) t).1;

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