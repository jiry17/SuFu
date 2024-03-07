Item = {Int, Int};
Inductive ItemList = cons {Item, ItemList} | nil Unit;
Plan = ItemList;
Inductive PlanList = consPlan {Compress Plan, PlanList} | nilPlan Unit;

max = \x: Int. \y: Int. if (< x y) then y else x;

/* A trivial program for 01knapsack */

step = \i: Item. fix (\f: PlanList -> PlanList. \ps: PlanList.
  match ps with
    consPlan {p, t} ->
      let res = f t in consPlan {cons {i, p}, consPlan {p, res}}
  | nilPlan _ -> ps
  end
);

gen = fix (
  \f: ItemList -> PlanList. \items: ItemList.
  match items with
    cons {i, t} ->
      let res = f t in step i res
  | nil _ -> consPlan {nil unit, nilPlan unit}
  end
);

getbest = let sumw = fix (
  \f: ItemList -> Int. \xs: ItemList.
  match xs with
    nil _ -> 0
  | cons {h, t} -> + h.1 (f t)
  end
) in let sumv = fix (
  \f: ItemList -> Int. \xs: ItemList.
  match xs with
    nil _ -> 0
  | cons {h, t} -> + h.2 (f t)
  end
) in \lim: Int. fix (
  \f: PlanList -> Int. \ps: PlanList.
  match ps with
    consPlan {p, t} ->
      let res = f t in
        if (< lim (sumw p)) then res else (max (sumv p) res)
  | nilPlan _ -> 0
  end
);

knapsack = \w: Int. \is: ItemList. getbest w (gen is);