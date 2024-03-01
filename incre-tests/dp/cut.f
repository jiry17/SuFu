Config SampleIntMin = 0;
Config SampleIntMax = 10;

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

/*Generate plans*/
size = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> + 1 (f t)
  end
);

generate = \full_xs: List. fix (
  \f: Int -> PlanList. \n: Int.
  if == n 0 then pcons {nil unit, pnil unit}
  else fix (
    \g: Int -> List -> PlanList. \m: Int. \xs: List.
    match xs with
      nil _ -> pnil unit
    | cons {h, t} ->
      let take = extend h (f (- n m)) in
      if < m n then merge take (g (+ m 1) t)
      else take
    end
  ) 1 full_xs
) (size full_xs);

/*Get best*/
max = \a: Int. \b: Int. if < a b then b else a;

get_best = let eval = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> + h (f t)
  end
) in fix (
  \f: PlanList -> Int. \xs: PlanList.
  match xs with
    pnil _ -> 0
  | pcons {h, t} -> max (eval h) (f t)
  end
);

/*Main program*/
main = \xs: List. get_best (generate xs);
/*xs = cons {1, cons {5, cons {8, cons {9, nil unit}}}};
main xs;
main 1;*/