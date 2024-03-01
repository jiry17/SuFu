Config SampleIntMin = 1;
Config SampleIntMax = 5;

Edge = {Int, Int};
Inductive EdgeList = enil Unit | econs {{Int, Int}, EdgeList};
Path = Compress EdgeList;
Inductive Graph = gnil Unit | gcons {EdgeList, Graph};
Inductive PlanList = pnil Unit | pcons {Path, PlanList};

/*Generate all valid paths*/

graph_size = fix (
  \f: Graph -> Int. \g: Graph.
  match g with
    gnil _ -> 0
  | gcons {h, t} -> + 1 (f t)
  end
);

is_valid_graph = \g: Graph.
  let size = graph_size g in
  fix (
    \f: Graph -> Int -> Bool. \g: Graph. \now: Int.
    match g with
      gnil _ -> false
    | gcons {enil _, gnil _} -> true
    | gcons {enil _, _} -> false
    | gcons {edges, remain} ->
      if f remain (+ 1 now) then fix (
        \f: EdgeList -> Bool. \xs: EdgeList.
        match xs with
          enil _ -> true
        | econs {{target, _}, tails} ->
          if or (<= target now) (>= target size) then false
          else f tails
        end
      ) edges else false
    end
  ) g 0;

generate_paths = \g: Graph.
  let size = graph_size g in
  fix (
    \f: Graph -> Int -> Int -> Path -> PlanList -> PlanList.
    \g: Graph. \now: Int. \next: Int. \p: Path. \current: PlanList.
    match g with
      gnil _ -> current
    | gcons {edges, remain} ->
      if < now next then f remain (+ now 1) next p current
      else if == now (- size 1) then pcons {p, current}
      else fix (
        \enum: EdgeList -> PlanList. \xs: EdgeList.
        match xs with
          enil _ -> current
        | econs {edge, tails} ->
          let res = enum tails in
            if or (<= edge.1 now) (>= edge.1 size) then res
            else f remain (+ now 1) edge.1 (econs {edge, p}) res
        end
      ) edges
    end
  ) g 0 0 (enil unit) (pnil unit);

/*Select the best solution*/

inf = 100;
min = \a: Int. \b: Int. if < a b then a else b;

get_best = let eval = fix (
  \f: Path -> Int. \p: Path.
  match p with
    enil _ -> 0
  | econs {{_, w}, t} -> + w (f t)
  end
) in fix (
  \f: PlanList -> Int. \xs: PlanList.
  match xs with
    pnil _ -> inf
  | pcons {h, t} -> min (eval h) (f t)
  end
);

/*Final Program*/
main = \g: Graph. if is_valid_graph g then get_best (generate_paths g) else 0;

/*graph1 = gcons {enil unit, gnil unit};
generate_paths graph1;
main graph1;

graph2 = gcons {econs {{1, 1}, enil unit}, gcons {enil unit, gnil unit}};
generate_paths graph2;
main graph2;

graph3 =
  gcons {econs {{1, 1}, econs {{2, 3}, enil unit}},
  gcons {econs {{2, 1}, enil unit},
  gcons {enil unit, gnil unit}}};
graph_size graph3;
generate_paths graph3;
main graph3;

graph4 =
  gcons {econs {{1, 2}, econs {{2, 3}, enil unit}},
  gcons {econs {{2, 2}, enil unit},
  gcons {enil unit, gnil unit}}};
generate_paths graph4;
main graph4;

main 1;*/