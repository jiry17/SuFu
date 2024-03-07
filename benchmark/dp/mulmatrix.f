Config SampleIntMin = 1;
Config SampleSize = 6;

Inductive List = nil Unit | cons {Int, List};
Inductive Tree = leaf {Int, Int} | node {Tree, Tree};
Plan = Compress Tree;
Inductive PlanList = pnil Unit | pcons {Plan, PlanList};

mulcost = \l: Int. \mid: Int. \r: Int. * (* l r) mid;

merge = fix (
  \f: PlanList -> PlanList -> PlanList.
  \xs: PlanList. \ys: PlanList.
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
    nil _ -> pnil unit
  | cons {_, nil _} -> pnil unit
  | cons {a, cons {b, nil _}} -> pcons {leaf {a, b}, pnil unit}
  | cons {h, t} -> fix (
      \g: List -> List -> PlanList. \l: List. \r: List.
      match r with
        cons {h, newr} ->
          let newl = snoc h l in
          let res = combine (f newl) (f r) in
            match newr with
              cons {w, nil _} -> res
            | _ -> merge res (g newl newr)
            end
      end
    ) (cons {h, nil unit}) t
  end
);

/*Get best*/
eval = \t: Tree. (fix (
  \f: Tree -> {Int, Int, Int}. \t: Tree.
  match t with
    leaf {l, r} -> {0, l, r}
  | node {l, r} ->
    let lres = f l in
    let rres = f r in
      {+ (+ lres.1 rres.1) (mulcost lres.2 lres.3 rres.3), lres.2, rres.3}
  end
) t).1;

min = \a: Int. \b: Int. if < a b then a else b;

get_best = fix (
  \f: PlanList -> Int. \xs: PlanList.
  match xs with
    pnil _ -> 20000
  | pcons {h, t} -> min (eval h) (f t)
  end
);

/*Main program*/
main = \xs: List. get_best (generate xs);
/*xs = cons {30, cons {35, cons {15, cons {5,
     cons {10, cons {20, cons {25, nil unit}}}}}}};
main xs;
main 1;*/