Config SampleIntMin = -1;
Config SampleSize = 20;
Inductive List = nil Unit | cons {Int, List};
Inductive Graph = gnil Unit | gcons {List, Graph};
Inductive Path = fin Unit | continue {Int, Int, Int, Path};
Plan = Compress Path;
Inductive PlanList = pnil Unit | pcons {Plan, PlanList};

graph_size = fix (
  \f: Graph -> Int. \g: Graph.
  match g with
    gnil _ -> 0
  | gcons {h, t} -> + 1 (f t)
  end
);

length = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> + 1 (f t)
  end
);

is_valid_graph = \g: Graph.
  let size = graph_size g in fix (
    \f: Graph -> Bool. \g: Graph.
    match g with
      gnil _ -> true
    | gcons {h, t} ->
      if == size (length h) then true else f t
    end
  ) g;

merge = fix (
  \f: PlanList -> PlanList -> PlanList. \xs: PlanList.
  \ys: PlanList.
  match xs with
    pnil _ -> ys
  | pcons {h, t} -> pcons {h, f t ys}
  end
);

/*Generate All Paths*/
generate = \g: Graph. let size = graph_size g in
fix (
  \f: Graph -> Int -> Int -> Plan -> PlanList.
  \graph: Graph. \now: Int. \next: Int. \path: Plan.
  match graph with
    gnil _ -> pnil unit
  | gcons {edges, remain} ->
    if < now next then f remain (+ now 1) next path
    else if == now (- size 1) then pcons {path, pnil unit}
    else fix (
      \g: List -> Int -> PlanList. \xs: List. \i: Int.
      match xs with
        nil _ -> pnil unit
      | cons {w, t} ->
        if or (<= i now) (== w -1) then g t (+ i 1)
        else merge (g t (+ i 1)) (f remain (+ now 1) i (continue {now, i, w, path}))
      end
    ) edges 0
  end
) g 0 0 (fin unit);

/*Evaluate*/

min = \a: Int. \b: Int. if < a b then a else b;
inf = 100;

get_best = let eval = fix (
  \f: Path -> Int. \xs: Path.
  match xs with
    fin _ -> 0
  | continue {a, b, w, t} -> + w (f t)
  end
) in fix (
  \f: PlanList -> Int. \xs: PlanList.
  match xs with
    pnil _ -> inf
  | pcons {h, t} -> min (eval h) (f t)
  end
);

/*main*/
main = \g: Graph. if is_valid_graph g then get_best (generate g) else 0;

/*graph1 = gcons {cons {-1, nil unit}, gnil unit};
main graph1;

graph2 = gcons {cons {-1, cons {1, nil unit}},
         gcons {cons {-1, cons {-1, nil unit}},
         gnil unit}};
main graph2;

graph3 = gcons {cons {-1, cons {1, cons {3, nil unit}}},
         gcons {cons {-1, cons {-1, cons {1, nil unit}}},
         gcons {cons {-1, cons {-1, cons {-1, nil unit}}},
         gnil unit}}};
main graph3;

graph4 = gcons {cons {-1, cons {2, cons {3, nil unit}}},
         gcons {cons {-1, cons {-1, cons {2, nil unit}}},
         gcons {cons {-1, cons {-1, cons {-1, nil unit}}},
         gnil unit}}};
main graph4;

main 1;*/