Config SampleIntMin = 0;
Config SampleIntMax = 10;
Config SampleSize = 6;

Inductive List = nil Unit | cons {Int, List};
Plan = Compress List;
Inductive PlanList = pnil Unit | pcons {Plan, PlanList};

merge = fix (
  \f: PlanList -> PlanList -> PlanList.
  \xs: PlanList. \ys: PlanList.
  match xs with
    pnil _ -> ys
  | pcons {h, t} -> pcons {h, f t ys}
  end
);

extend = \w: Int. fix (
  \f: PlanList -> PlanList. \xs: PlanList.
  match xs with
    pnil _ -> xs
  | pcons {h, t} -> pcons {cons {w, h}, f t}
  end
);

/*Generate*/
generate = fix (
  \f: List -> List -> PlanList. \xs: List. \ys: List.
  match {xs, ys} with
    {cons {h1, t1}, cons {h2, t2}} ->
      let res1 = merge (f t1 ys) (f xs t2) in
      if == h1 h2 then
        merge (extend h1 (f t1 t2)) res1
      else res1
  | _ -> pcons {nil unit, pnil unit}
  end
);

/*Get best*/
max = \a: Int. \b: Int. if < a b then b else a;
one = 1;

get_best = let eval = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> + 1 (f t)
  end
) in fix (
  \f: PlanList -> Int. \xs: PlanList.
  match xs with
    pnil _ -> 0
  | pcons {h, t} -> max (eval h) (f t)
  end
);

/*Main program*/
main = \xs: List. \ys: List. get_best (generate xs ys);
/*xs1 = cons {1, cons {2, cons {3, nil unit}}};
ys1 = cons {1, cons {2, cons {3, nil unit}}};
main xs1 ys1;

xs2 = cons {1, cons {2, cons {4, nil unit}}};
ys2 = cons {1, cons {3, cons {4, nil unit}}};
main xs2 ys2;

xs3 = cons {1, cons {3, cons {4, cons {5, nil unit}}}};
ys3 = cons {1, cons {2, cons {3, cons {5, nil unit}}}};
main xs3 ys3;
main 1;*/