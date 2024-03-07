Inductive List = nil Unit | cons {Int, List};
Inductive Graph = gnil Unit | gcons {List, Graph};
Plan = Compress {List, List};
Inductive PlanList = pnil Unit | pcons {Plan, PlanList};

head = \e: Int. \xs: List.
  match xs with
    nil _ -> e
  | cons {h, t} -> h
  end;

graph_size = fix (
  \f: Graph -> Int. \g: Graph.
  match g with
    gnil _ -> 0
  | gcons {h, t} -> + 1 (f t)
  end
);

size = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> + 1 (f t)
  end
);

is_valid_graph = \g: Graph.
  let si = graph_size g in fix (
    \f: Graph -> Bool. \g: Graph.
    match g with
      gnil _ -> true
    | gcons {h, t} ->
      if == si (size h) then f t else false
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

one = 1;

extend = \pos: Int. \w: Int. fix (
  \f: PlanList -> PlanList. \xs: PlanList.
  match xs with
    pnil _ -> pnil unit
  | pcons {h, t} ->
    pcons {{cons {pos, h.1}, cons {w, h.2}}, f t}
  end
);

generate_paths = fix (
  \f: Graph -> PlanList. \graph: Graph.
  match graph with
    gnil _ -> pcons {{nil unit, nil unit}, pnil unit}
  | gcons {edges, remains} ->
    let sub_res = f remains in
    fix (
      \g: Int -> List -> PlanList. \pos: Int. \xs: List.
      match xs with
        nil _ -> pnil unit
      | cons {h, t} ->
        merge (extend pos h sub_res) (g (+ 1 pos) t)
      end
    ) 0 edges
  end
);

/*evaluate*/

sum = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> + h (f t)
  end
);

is_val = let check = fix (
  \f: List -> Int -> Bool. \xs: List. \pre: Int.
  match xs with
    nil _ -> true
  | cons {h, t} -> and (and (<= h (+ 1 pre)) (>= h (- pre 1))) (f t h)
  end
) in \xs: List. match xs with
    nil _ -> true
  | cons {h, t} -> check t h
  end;


min = \a: Int. \b: Int. if > a b then b else a;

get_best = fix (
  \f: PlanList -> Int. \xs: PlanList.
  match xs with
    pnil _ -> 1000
  | pcons {h, t} ->
    if is_val h.1 then min (sum h.2) (f t) else f t
  end
);

/*Get result*/
main = \graph: Graph.
  if (is_valid_graph graph) then get_best (generate_paths graph) else 0;
/*graph =
  gcons {cons {1, cons {3, cons {5, nil unit}}},
  gcons {cons {7, cons {4, cons {1, nil unit}}},
  gcons {cons {1, cons {3, cons {5, nil unit}}},
  gnil unit}}};
fix (
  \f: PlanList -> Graph. \xs: PlanList.
  match xs with
    pnil _ -> gnil unit
  | pcons {h, t} ->
    if is_val h.1 then gcons {h.1, f t} else f t
  end
) (generate_paths graph);
main graph;
main 1;*/