Config SampleIntMin = 0;
Config NonLinear = true;

Inductive List = nil Unit | cons {Int, List};
Inductive LineList = lnil Unit | lcons {List, LineList};
Plan = LineList;
Inductive PlanList = pnil Unit | pcons {Compress Plan, PlanList};

@Input lim: Int;
/*xs = cons {1, cons {4, cons {1, nil unit}}};*/

size = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> + 1 (f t)
  end
);

sum = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> + h (f t)
  end
);

line_cost =
  let triple = \x: Int. * (* x x) x in
  \xs: List.
  let len = (- (+ (sum xs) (size xs)) 1) in
    triple (- lim len);

/*Generate all solutions*/

merge = fix (
  \f: PlanList -> PlanList -> PlanList.
  \xs: PlanList. \ys: PlanList.
  match xs with
    pnil _ -> ys
  | pcons {h, t} -> pcons {h, f t ys}
  end
);

head_size = \xs: Plan.
  match xs with
    lnil _ -> + lim 1
  | lcons {h, t} -> + (sum h) (size h)
  end;

extend = \x: Int. \xs: PlanList.
if > x lim then pnil unit else
fix (
  \f: PlanList -> PlanList. \xs: PlanList.
  match xs with
    pnil _ -> xs
  | pcons {h, t} ->
    let res = f t in
    let hcost = head_size h in
    let new_plan = lcons {cons {x, nil unit}, h} in
    if <= (+ x hcost) lim then
      let insert_plan = match h with
        lcons {h, t} -> lcons {cons {x, h}, t}
      end in
        pcons {insert_plan, pcons {new_plan, res}}
    else pcons {new_plan, res}
  end
) xs;

generate = fix (
  \f: List -> PlanList. \xs: List.
  match xs with
    nil _ -> pcons {lnil unit, pnil unit}
  | cons {h, t} -> extend h (f t)
  end
);

/*Get best solution*/
eval = fix (
  \f: Plan -> Int. \xs: Plan.
  match xs with
    lnil _ -> 0
  | lcons {h, lnil _} -> 0
  | lcons {h, t} -> + (line_cost h) (f t)
  end
);

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
/*generate xs;
main xs;
main 1;*/