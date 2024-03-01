Item = {Int, Int};
Inductive ItemList = cons {Item, ItemList} | nil Unit;
Plan = ItemList;
Inductive PlanList = consPlan {Plan, PlanList} | nilPlan Unit;

max = lambda x: Int. lambda y: Int. if (< x y) then y else x;

fold = lambda f: Item -> Int -> Int. lambda x: ItemList. lambda w: Int.
    fix (lambda g: ItemList -> Int. lambda x: ItemList.
        match x with
          cons {i, t} -> f i (g t)
        | nil _ -> w 
        end
    ) x;

sumw = lambda x: Plan. fold (lambda a: Item. lambda b: Int. + a.1 b) x 0;
sumv = lambda x: Plan. fold (lambda a: Item. lambda b: Int. + a.2 b) x 0;

/* A trivial program for 01knapsack */

step = lambda i: Item. fix (lambda f: PlanList -> PlanList. lambda ps: PlanList.
    match ps with
      consPlan {p, t} ->
        let res = f t in consPlan {cons {i, p}, consPlan {p, res}}
    | nilPlan _ -> ps
    end
);

gen = fix (
    lambda f: ItemList -> PlanList. lambda items: ItemList.
    match items with 
      cons {i, t} ->
        let res = f t in step i res
    | nil _ -> consPlan {nil unit, nilPlan unit}
    end    
);

getbest = lambda lim: Int. fix (
    lambda f: PlanList -> Int. lambda ps: PlanList.
    match ps with
      consPlan {p, t} ->
        let res = f t in
            if (< lim (sumw p)) then res else (max (sumv p) res)
    | nilPlan _ -> 0
    end
);

knapsack = lambda w: Int. lambda is: ItemList. getbest w (gen is);

/* Compress program for 01knapsack*/
Inductive CPlanList = consCP {Compress Plan, CPlanList} | nilCP Unit; 

stepC = lambda i: Item. fix (lambda f: CPlanList -> CPlanList. lambda ps: CPlanList.
    match ps with
      consCP {cp, t} ->
        let new = cons {i, cp} in 
          let res = f t in 
            consCP {new, consCP {cp, res}}
    | nilCP _ -> ps
    end
);

genC = fix (
    lambda f: ItemList -> CPlanList. lambda items: ItemList.
    match items with 
      cons {i, t} ->
        let res = f t in stepC i res
    | nil _ -> consCP {nil unit, nilCP unit}
    end    
);

getbestC = lambda lim: Int. fix (
    lambda f: CPlanList -> Int. lambda ps: CPlanList.
    match ps with
      consCP {pc, t} ->
        let now = {sumw pc, sumv pc} in 
          let res = f t in 
            if (< lim now.1) then res else (max now.2 res)
    | nilCP _ -> 0
    end
);

knapsackC = lambda w: Int. lambda is: ItemList. getbestC w (genC is);

/* Tests 
l = cons {{3, 3}, cons {{2, 2}, cons {{1,2}, nil unit}}};
knapsack 1 l;
knapsack 2 l;
knapsack 3 l;
knapsack 4 l;

knapsackC 1 l;
knapsackC 2 l;
knapsackC 3 l;
knapsackC 4 l;*/