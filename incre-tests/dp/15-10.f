Config SampleIntMax = 3;
Config ComposeNum = 6;
Config NonLinear = true;

Inductive List = nil Unit | cons {Int, List};
Inductive Graph = gnil Unit | gcons {List, Graph};
Inductive Choice = cnil Unit | ccons {Int, Int, Choice};
Plan = Compress Choice;
Inductive PlanList = pnil Unit | pcons {Plan, PlanList};

@Input cost1: Int;
@Input cost2: Int;

head = \xs: Choice. match xs with
  cnil _ -> {-1, -1}
| ccons {a, b, t} -> {a, b}
end;

merge = fix (
  \f: PlanList -> PlanList -> PlanList. \xs: PlanList.
  \ys: PlanList.
  match xs with
    pnil _ -> ys
  | pcons {h, t} -> pcons {h, f t ys}
  end
);

snoc = \pos: Int. \w: Int. fix (
  \f: Choice -> Choice. \xs: Choice.
  match xs with
    cnil _ -> ccons {pos, w, cnil unit}
  | ccons {a, b, t} -> ccons {a, b, f t}
  end
);

extend = \pos: Int. \w: Int. fix (
  \f: PlanList -> PlanList. \xs: PlanList.
  match xs with
    pnil _ -> pnil unit
  | pcons {h, t} ->
    pcons {snoc pos w h, f t}
  end
);

take_last = fix (
  \f: Graph -> {List, Graph}. \g: Graph.
  match g with
    gnil _ -> {nil unit, g}
  | gcons {h, gnil _} -> {h, gnil unit}
  | gcons {h, t} ->
    let res = f t in {res.1, gcons {h, res.2}}
  end
);

generate_paths = fix (
  \f: Graph -> PlanList. \graph: Graph.
  match graph with
    gnil _ -> pcons {cnil unit, pnil unit}
  | _ -> let split = take_last graph in
    let sub_res = f split.2 in
    fix (
      \g: Int -> List -> PlanList. \pos: Int. \xs: List.
      match xs with
        nil _ -> pnil unit
      | cons {h, t} ->
        merge (extend pos h sub_res) (g (+ 1 pos) t)
      end
    ) 0 split.1
  end
);

/*Get best plan*/

eval = fix (
  \f: Int -> Int -> Choice -> Int. \pre: Int. \now: Int. \xs: Choice.
  match xs with
    cnil _ -> now
  | ccons {a, b, t} ->
    let now =
      if == pre -1 then * b now else
      if == a pre then * b (- now cost1) else * b (- now cost2)
    in f a now t
  end
) -1 1;

max = \a: Int. \b: Int. if < a b then b else a;

get_best = fix (
  \f: PlanList -> Int. \xs: PlanList.
  match xs with
    pnil _ -> 0
  | pcons {h, t} -> max (eval h) (f t)
  end
);

/*Main program*/
main = \graph: Graph. get_best (generate_paths graph);
/*graph = gcons {cons {1, cons {2, nil unit}},
        gcons {cons {3, cons {1, nil unit}},
        gnil unit}};
generate_paths graph;
main graph;
main 1;*/