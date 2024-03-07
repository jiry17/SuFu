Config SampleIntMin = 0;
Config ComposeNum = 5;

Inductive List = nil Unit | cons {Int, List};
Point = {Int, Int};
Inductive PointList = ponil Unit | pocons {Point, PointList};
Plan = {PointList, PointList};
Inductive PlanList = pnil Unit | pcons {Compress Plan, PlanList};

sqr = \x: Int. * x x;
dis = \x: Point. \y: Point. + (sqr (- x.1 y.1)) (sqr (- x.2 y.2));

head = \xs: PointList.
  match xs with
    ponil _ -> {0, 0}
  | pocons {h, _} -> h
  end;

extend = \p: Point. fix (
  \f: PlanList -> PlanList. \xs: PlanList.
  match xs with
    pnil _ -> xs
  | pcons {h, t} ->
    let h1 = {pocons {p, h.1}, h.2} in
    let h2 = {h.1, pocons {p, h.2}} in
      pcons {h1, pcons {h2, f t}}
  end
);

/*Generate All Plans*/
generate = fix (
  \f: Int -> List -> PlanList. \pos: Int. \xs: List.
  match xs with
    nil _ -> pnil unit
  | cons {w, nil _} ->
    let polist = pocons {{pos, w}, ponil unit} in
      pcons {{polist, polist}, pnil unit}
  | cons {h, t} ->
    let res = f (+ pos 1) t in extend {pos, h} res
  end
) 0;

/*Evaluate*/

eval_plist = fix (
  \f: Point -> PointList -> Int. \pre: Point. \xs: PointList.
  match xs with
    ponil _ -> 0
  | pocons {p, t} -> + (dis p pre) (f p t)
  end
);

min = \a: Int. \b: Int. if < a b then a else b;

get_best =
  let eval = \p: Plan.
    let first = head p.1 in
      + (eval_plist first p.1) (eval_plist first p.2)
  in fix (
   \f: PlanList -> Int. \xs: PlanList.
   match xs with
     pnil _ -> 1000
   | pcons {h, t} -> min (eval h) (f t)
   end
  );

/*Main*/
main = \xs: List. get_best (generate xs);

/*example1 = cons {1, cons {5, cons {3, nil unit}}};
main example1;
example2 = cons {1, cons {4, cons {2, cons {3, nil unit}}}};
main example2;
example3 = cons {1, cons {2, cons {3, cons {4, cons {5, nil unit}}}}};
main example3;

main 1;*/

