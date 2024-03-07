Config SampleSize = 4;

@Input cost_list: {Int, Int, Int, Int, Int, Int};
/*cost_list = {1, 1, 1, 1, 1, 1};*/

Inductive Op =
  copy Unit | replace Unit | delete Unit | insert Unit | rotate Unit | kill Unit;
Inductive OpList = onil Unit | ocons {Op, OpList};
Plan = OpList;
Inductive PlanList = pnil Unit | pcons {Compress Plan, PlanList};
Inductive List = nil Unit | cons {Int, List};

get_cost = \op: Op.
  match op with
    copy _ -> cost_list.1
  | replace _ -> cost_list.2
  | delete _ -> cost_list.3
  | insert _ -> cost_list.4
  | rotate _ -> cost_list.5
  | kill _ -> cost_list.6
  end;

extend = \op: Op. fix (
  \f: PlanList -> PlanList. \xs: PlanList.
  match xs with
    pnil _ -> xs
  | pcons {h, t} -> pcons {ocons {op, h}, f t}
  end
);

merge = fix (
  \f: PlanList -> PlanList -> PlanList. \xs: PlanList. \ys: PlanList.
  match xs with
    pnil _ -> ys
  | pcons {h, t} -> pcons {h, f t ys}
  end
);

/*Generate all solutions*/
size_up_2 = \xs: List.
  match xs with
    nil _ -> 0
  | cons {_, nil _} -> 1
  | _ -> 2
  end;

unfold = \xs: List.
  match xs with
    nil _ -> {0, xs}
  | cons {h, t} -> {h, t}
  end;

generate = fix (
  \f: List -> List -> PlanList. \xs: List. \ys: List.
  let xsize = size_up_2 xs in
  let ysize = size_up_2 ys in
  if and (== xsize 0) (== ysize 0) then
    pcons {onil unit, pnil unit}
  else let res0 =
    if and (> xsize 0) (> ysize 0) then
      let xinfo = unfold xs in let yinfo = unfold ys in
      let tail_res = f xinfo.2 yinfo.2 in
      let res0 = extend (replace unit) tail_res in
        if == xinfo.1 yinfo.1 then
          merge res0 (extend (copy unit) tail_res)
        else res0
    else pnil unit
  in let res1 =
    if == xsize 0 then pnil unit
    else let xinfo = unfold xs in
      extend (delete unit) (f xinfo.2 ys)
  in let res2 =
    if == ysize 0 then pnil unit
    else let yinfo = unfold ys in
      extend (insert unit) (f xs yinfo.2)
  in let res3 =
    if or (< xsize 2) (< ysize 2) then pnil unit
    else let xinfo = unfold xs in let xxinfo = unfold xinfo.2 in
    let yinfo = unfold ys in let yyinfo = unfold yinfo.2 in
      if and (== xinfo.1 yyinfo.1) (== xxinfo.1 yinfo.1) then
        extend (rotate unit) (f xxinfo.2 yyinfo.2)
      else pnil unit
  in let res4 =
    if (== ysize 0) then
      let kill_plan = ocons {kill unit, onil unit} in
        pcons {kill_plan, pnil unit}
    else pnil unit in
    merge res4 (merge (merge res0 res1) (merge res2 res3))
);

/*Select the best plan*/
min = \a: Int. \b: Int. if < a b then a else b;

get_best = let eval = fix (
  \f: Plan -> Int. \xs: Plan.
  match xs with
    onil _ -> 0
  | ocons {h, t} -> + (get_cost h) (f t)
  end
) in fix (
  \f: PlanList -> Int. \xs: PlanList.
  match xs with
    pnil _ -> 1000
  | pcons {h, t} -> min (eval h) (f t)
  end
);

/*Main program*/
main = \xs: List. \ys: List. get_best (generate xs ys);