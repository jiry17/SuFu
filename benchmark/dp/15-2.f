Config SampleIntMin = 0;
Config SampleSize = 6;
Inductive List = nil Unit | cons {Int, List};
Plan = Compress List;
Inductive PlanList = pnil Unit | pcons {Plan, PlanList};

take_back = fix (
  \f: List -> {Int, List}. \xs: List.
  match xs with
    nil _ -> {-1, nil unit}
  | cons {h, nil _} -> {h, nil unit}
  | cons {h, t} ->
    let res = f t in {res.1, cons {h, res.2}}
  end
);

snoc = \w: Int. fix (
  \f: List -> List. \xs: List.
  match xs with
    nil _ -> cons {w, nil unit}
  | cons {h, t} -> cons {h, f t}
  end
);

rev = fix (
  \f: List -> List -> List. \pre: List. \xs: List.
  match xs with
    nil _ -> pre
  | cons {h, t} -> f (cons {h, pre}) t
  end
) (nil unit);

merge = fix (
  \f: PlanList -> PlanList -> PlanList. \xs: PlanList.
  \ys: PlanList.
  match xs with
    pnil _ -> ys
  | pcons {h, t} -> pcons {h, f t ys}
  end
);

extend = \op: Plan -> Plan. fix (
  \f: PlanList -> PlanList. \xs: PlanList.
  match xs with
    pnil _ -> xs
  | pcons {h, t} -> pcons {op h, f t}
  end
);

/*Generate*/
generate = fix (
  \f: List -> PlanList. \xs: List.
  match xs with
    nil _ -> pcons {nil unit, pnil unit}
  | cons {h, nil _} -> pcons {xs, pcons {nil unit, pnil unit}}
  | cons {h, t} ->
    let last = take_back t in
    let res = merge (f t) (f (cons {h, last.2})) in
      if == h last.1 then
        merge (extend (\xs: Plan. cons {h, snoc last.1 xs}) (f last.2)) res
      else res
  end
);

/*Select best*/
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

/*Main*/
main = \xs: List. get_best (generate xs);

/*
main (cons {1, nil unit});
main (cons {1, cons {1, nil unit}});
main (cons {1, cons {2, cons {1, nil unit}}});
main (cons {1, cons {2, cons {2, cons {1, cons {1, cons {2, nil unit}}}}}});
main (cons {3, cons {3, cons {1, cons {2, cons {3, cons {4, cons {2, cons {1, cons {4, cons {4, nil unit}}}}}}}}}});
main 1;*/