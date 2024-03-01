Config SampleSize = 7;
Item = {Int, Int};
Inductive ItemList = cons {Item, ItemList} | nil Unit;
Plan = ItemList;
Inductive PlanList = consPlan {Compress Plan, PlanList} | nilPlan Unit;

max = \x: Int. \y: Int. if (< x y) then y else x;

/* A trivial program for 01knapsack */

step = \i: Item. fix (\f: PlanList -> PlanList. \ps: PlanList.
  match ps with
    consPlan {p, t} ->
      let res = f t in
      let newp = align (label (cons {i, unlabel p})) in
      consPlan {newp, consPlan {p, res}}
  | nilPlan _ -> ps
  end
);

gen = fix (
  \f: ItemList -> PlanList. \items: ItemList.
  match items with
    cons {i, t} ->
      let res = f t in step i res
  | nil _ -> consPlan {align (label (nil unit)), nilPlan unit}
  end
);

getbest =
let sumw = fix (
  \f: ItemList -> Int. \xs: ItemList.
  match xs with
    nil _ -> 0
  | cons {h, t} -> + h.1 (f t)
  end
) in
let sumv = fix (
  \f: ItemList -> Int. \xs: ItemList.
  match xs with
    nil _ -> 0
  | cons {h, t} -> + h.2 (f t)
  end
) in
\lim: Int. fix (
  \f: PlanList -> Compress Plan. \ps: PlanList.
  match ps with
    consPlan {p, nilPlan _} -> p
  | consPlan {p, t} ->
      let res = f t in
      let nowv = align (sumv (unlabel p)) in
      if (< lim nowv) then res
      else let resw = align (sumw (unlabel res)) in
      let noww = align (sumw (unlabel p)) in
      if < resw noww then p else res
  end
);

knapsack = \w: Int. \is: ItemList. align (unlabel (getbest w (gen is)));