Config SampleIntMin = 0;
Config SampleSize = 9;

Inductive List = nil Unit | cons {Int, List};
Inductive EdgeList = enil Unit | econs {Int, Int, EdgeList};
Inductive Graph = gnil Unit | gcons {EdgeList, Graph};
Plan = Compress {List, List};
Inductive PlanList = pnil Unit | pcons {Plan, PlanList};

graph_size = fix (
  \f: Graph -> Int. \g: Graph.
  match g with
    gnil _ -> 0
  | gcons {h, t} -> + 1 (f t)
  end
);

length = fix (
  \f: EdgeList -> Int. \xs: EdgeList.
  match xs with
    enil _ -> 0
  | econs {_, _, t} -> + 1 (f t)
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
  let size = graph_size g in fix (
    \f: Graph -> Bool. \g: Graph.
    match g with
      gnil _ -> true
    | gcons {h, t} ->
      if == size (length h) then f t else false
    end
  ) g;

@Input cs = cons {0, cons {0, cons {0, nil unit}}};
@Input len = 3;
/*@Input graph: Graph;
@Input cs: List;*/

take_last = \xs: List. \num: Int.
(fix (
  \f: List -> {Int, List}. \xs: List.
  match xs with
    nil _ -> {0, xs}
  | cons {h, t} ->
    let res = f t in
      if < res.1 num then {+ res.1 1, cons {h, res.2}}
      else res
  end
) xs).2;

/*Generate all paths*/

merge = fix (
  \f: PlanList -> PlanList -> PlanList. \xs: PlanList.
  \ys: PlanList.
  match xs with
    pnil _ -> ys
  | pcons {h, t} -> pcons {h, f t ys}
  end
);

extend = \c: Int. \w: Int. fix (
  \f: PlanList -> PlanList. \xs: PlanList.
  match xs with
    pnil _ -> pnil unit
  | pcons {h, t} -> pcons {{cons {c, h.1}, cons {w, h.2}}, f t}
  end
);

generate_paths = \graph: Graph. fix (
  \f: Graph -> Int -> PlanList. \g: Graph. \now: Int.
  if == now len then pcons {enil unit, pnil unit}
  else match g with
    gnil _ -> pnil unit
  | gcons {edges, remain} -> fix (
      \g: EdgeList -> PlanList. \xs: EdgeList.
      match xs with
        enil _ -> pnil unit
      | econs {a, b, t} ->
        merge (extend a b (f remain (+ now 1))) (g t)
      end
    )
  end
) graph 0;

/*evaluate*/

list_eq = fix (
  \f: List -> List -> Bool. \xs: List. \ys: List.
  match {xs, ys} with
    {nil _, nil _} -> true
  | {cons {h1, t1}, cons {h2, t2}} -> and (== h1 h2) (f t1 t2)
  | _ -> false
  end
);

eval = fix (
  \f: List -> Int. \path: List.
  match path with
    cons {h, t} -> + h (f t)
  | nil _ -> 0
  end
);

max = \a: Int. \b: Int. if < a b then b else a;

get_best = fix (
  \f: PlanList -> Int. \xs: PlanList.
  match xs with
    pnil _ -> 0
  | pcons {h, t} ->
    if list_eq cs h.1 then max (eval h.2) (f t)
    else f t
  end
);

/*Get result*/
main = \graph: Graph.
  if and (is_valid_graph graph) (== (size cs) len) then get_best (generate_paths graph) else 0;
graph =
  gcons {econs {0, 1, econs {0, 1, econs {0, 2, enil unit}}},
  gcons {econs {1, 1, econs {0, 3, econs {2, 1, enil unit}}},
  gcons {econs {1, 1, econs {1, 1, econs {0, 1, enil unit}}},
  gnil unit}}};
main graph;
main 1;