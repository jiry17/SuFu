Config SampleIntMin = 0;
Config SampleSize = 4;
Config NonLinear = true;

Inductive List = nil Unit | cons {Int, List};
Inductive Schedule = fin Unit | month {Int, Int, Schedule};
Plan = Compress Schedule;
Inductive PlanList = pnil Unit | pcons {Plan, PlanList};

@Input cost: List;
@Input lim: Int; @Input m: Int; @Input c: Int;
inf = 100;

@Combine get_cost = \now: Int.
  fix (
    \f: List -> Int -> Int. \xs: List. \i: Int.
    match xs with
      nil _ -> inf
    | cons {h, t} ->
        if == i now then h else f t (+ i 1)
    end
  ) cost 0;

snoc = \target: Int. \w: Int. fix (
  \f: Schedule -> Schedule. \xs: Schedule.
  match xs with
    fin _ -> month {target, w, fin unit}
  | month {a, b, t} -> month {a, b, f t}
  end
);

length = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> + 1 (f t)
  end
);

pop_back = fix (
  \f: List -> {List, Int}. \xs: List.
  match xs with
    nil _ -> {xs, -1}
  | cons {h, nil _ } -> {nil unit, h}
  | cons {h, t} ->
    let res = f t in {cons {h, res.1}, res.2}
  end
);

merge = fix (
  \f: PlanList -> PlanList -> PlanList.
  \xs: PlanList. \ys: PlanList.
  match xs with
    pnil _ -> ys
  | pcons {h, t} -> pcons {h, f t ys}
  end
);

extra_cost = \new: Int. if > new m then * (- new m) c else 0;

remain = fix (
  \f: Schedule -> Int. \xs: Schedule.
  match xs with
    fin _ -> 0
  | month {target, new , t} -> - (+ (f t) new) target
  end
);

eval = fix (
  \f: Int -> Schedule -> Int. \pre: Int. \xs: Schedule.
  match xs with
    fin _ -> 0
  | month {target, new, t} ->
    let remain = - (+ pre new) target in
      + (f remain t) (+ (extra_cost new) (get_cost remain))
  end
) 0;

extend = \target: Int. \now: Int. fix (
  \f: PlanList -> PlanList. \xs: PlanList.
  match xs with
    pnil _ -> pnil unit
  | pcons {h, t} ->
    pcons {snoc target now h, f t}
  end
);

/*Generate all*/
generate = fix (
  \f: List -> PlanList. \xs: List.
  match xs with
    nil _ -> pcons {fin unit, pnil unit}
  | _ -> let split = pop_back xs in
    let subres = f split.1 in
    fix (
      \g: Int -> PlanList. \now: Int.
      if > now lim then pnil unit
      else merge (extend split.2 now subres) (g (+ now 1))
    ) 0
  end
);

/*Get best*/
extra_cost = \new: Int. if > new m then * (- new m) c else 0;

remain = fix (
  \f: Schedule -> Int. \xs: Schedule.
  match xs with
    fin _ -> 0
  | month {target, new , t} -> - (+ (f t) new) target
  end
);

eval = fix (
  \f: Int -> Schedule -> Int. \pre: Int. \xs: Schedule.
  match xs with
    fin _ -> 0
  | month {target, new, t} ->
    let remain = - (+ pre new) target in
      + (f remain t) (+ (extra_cost new) (get_cost remain))
  end
) 0;

min = \a: Int. \b: Int. if < a b then a else b;

get_best = fix (
  \f: PlanList -> Int. \xs: PlanList.
  match xs with
    pnil _ -> inf
  | pcons {h, t} -> min (f t) (eval h)
  end
);

/*Main*/
main = \xs: List. get_best (generate xs);


