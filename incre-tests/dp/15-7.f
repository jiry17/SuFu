Config SampleIntMin = 0;
Config SampleSize = 9;

Inductive List = nil Unit | cons {Int, List};
Inductive EdgeList = enil Unit | econs {Int, Int, EdgeList};
Inductive Graph = gnil Unit | gcons {EdgeList, Graph};
Plan = Compress EdgeList;
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

@Input cs: List;
@Input len: Int;

one = 1;

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

extend = \c: Int. \w: Int. \target: Int. fix (
  \f: PlanList -> PlanList. \xs: PlanList.
  match xs with
    pnil _ -> pnil unit
  | pcons {h, t} ->
    pcons {econs {c, w, h}, f t}
  end
);

generate_paths = \full_graph: Graph.
let gen = fix (
  \f: EdgeList -> Graph -> List -> PlanList. \xs: EdgeList. \graph: Graph. \cs: List.
  match cs with
    nil _ -> pcons {enil unit, pnil unit}
  | cons {c, ct} ->
    match {xs, graph} with
      {econs {a, b, t}, gcons {next_edges, remains}} ->
        merge (extend a b c (f next_edges full_graph ct)) (f t remains cs)
    | _ -> pnil unit
    end
  end
) in match full_graph with
  gnil _ -> pnil unit
| gcons {h, _} -> gen h full_graph cs
end;

/*evaluate*/

is_path_match = fix (
  \f: EdgeList -> List -> Bool. \p: EdgeList. \xs: List.
  match {p, xs} with
    {enil _, nil _} -> true
  | {econs {_, h1, t1}, cons {h2, t2}} -> and (== h1 h2) (f t1 t2)
  | _ -> false
  end
);

eval = fix (
  \f: EdgeList -> Int. \path: EdgeList.
  match path with
    econs {_, w, t} -> + w (f t)
  | enil _ -> 0
  end
);

max = \a: Int. \b: Int. if < a b then b else a;

get_best = fix (
  \f: PlanList -> Int. \xs: PlanList.
  match xs with
    pnil _ -> 0
  | pcons {h, t} ->
    if is_path_match h cs then max (eval h) (f t) else f t
  end
);

/*Get result*/
main = \graph: Graph.
  if and (is_valid_graph graph) (== (size cs) len) then get_best (generate_paths graph) else 0;
/*graph =
  gcons {econs {0, 1, econs {0, 1, econs {0, 2, enil unit}}},
  gcons {econs {1, 1, econs {0, 3, econs {2, 1, enil unit}}},
  gcons {econs {1, 1, econs {1, 1, econs {0, 1, enil unit}}},
  gnil unit}}};
generate_paths graph;
fix (
  \f: PlanList -> Int. \xs: PlanList.
  match xs with
    pnil _ -> 0
  | pcons {h, t} -> + 1 (f t)
  end
) (generate_paths graph);
main graph;
main 1;*/