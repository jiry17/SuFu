Config SampleIntMin = 0;
Config SampleIntMax = 5;

Player = {Int, Int};
Inductive List = nil Unit | cons {Player, List};
Inductive NList = nnil Unit | ncons {List, NList};
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

extend = \p: Player. fix (
  \f: PlanList -> PlanList. \xs: PlanList.
  match xs with
    pnil _ -> xs
  | pcons {h, t} -> pcons {cons {p, h}, f t}
  end
);

@Input lim: Int;

/*Generate results*/
generate = fix (
  \f: NList -> PlanList. \xs: NList.
  match xs with
    nnil _ -> pcons {nil unit, pnil unit}
  | ncons {players, remains} ->
    let res = f remains in
    fix (
      \g: List -> PlanList. \ys: List.
      match ys with
        nil _ -> pnil unit
      | cons {h, t} -> merge (extend h res) (g t)
      end
    ) players
  end
);

/*Get results*/
max = \a: Int. \b: Int. if < a b then b else a;

get_best = let sumw = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {p, t} -> + p.1 (f t)
  end
) in let sumv = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {p, t} -> + p.2 (f t)
  end
) in fix (
  \f: PlanList -> Int. \xs: PlanList.
  match xs with
    pnil _ -> 0
  | pcons {h, t} ->
    if <= (sumw h) lim then
      max (sumv h) (f t)
    else f t
  end
);

/*Main program*/
main = \xs: NList. get_best (generate xs);
/*xs = ncons {cons {{1, 2}, cons {{2, 5}, nil unit}},
     ncons {cons {{2, 10}, cons {{1, 2}, nil unit}},
     nnil unit}};
main xs;
main 1;*/

