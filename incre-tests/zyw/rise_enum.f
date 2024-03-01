Inductive List = nil Unit | cons {Int, List};
Inductive LList = lnil Unit | lcons {List, LList};

max = \x: Int. \y: Int. if > x y then x else y;

length = fix (\f: List->Int. \x: List.
  match x with
    cons {h, t} -> + (f t) 1
  | nil _ -> 0
  end
);

step = \item: Int. fix (\f: LList -> LList. \l: LList.
    match l with
      lnil _ -> l
    | lcons {h, t} ->
        let res = f t in lcons {cons {item, h}, lcons {h, res}}
    end
);

gen = fix (
    \f: List -> Compress LList. \items: List.
    match items with
      nil _ -> lcons {nil unit, lnil unit}
    | cons {i, t} ->
        let res = f t in step i res
    end
);

cal_constraint = fix (\f: List -> Int -> Bool. \pos: List. \pre: Int.
    match pos with
      nil _ -> true
    | cons {h, t} -> and (f t h) (< pre h)
    end
);

get_best = fix (\f: LList -> Int. \l: LList.
    match l with
      lnil _ -> 0
    | lcons {h, t} -> 
        let res = f t in
        if (cal_constraint h -100) then (max res (length h)) else res
    end
);

spec = \xs: List.
    get_best (gen xs);

main = \xs: List. spec xs;