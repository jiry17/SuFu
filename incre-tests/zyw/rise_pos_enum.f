Inductive List = nil Unit | cons {Int, List};
Inductive LList = lnil Unit | lcons {Compress List, LList};

max = \x: Int. \y: Int. if > x y then x else y;

length = fix (\f: List->Int. \x: List.
  match x with
    cons {h, t} -> + (f t) 1
  | nil _ -> 0
  end
);

sum = fix (\f: List -> Int. \x: List.
    match x with
      nil _ -> 0
    | cons {h, t} -> + h (f t)
    end
);

step = fix (\f: LList -> LList. \l: LList.
    match l with
      lnil _ -> l
    | lcons {h, t} ->
        let res = f t in
        lcons {cons {1, h}, lcons {cons {0, h}, res}}
    end
);

gen = fix (
    \f: Int -> LList. \len: Int.
    if (== len 0)
    then lcons {nil unit, lnil unit}
    else let l = f (- len 1) in step l
);

access = fix (\f: List -> Int -> Int. \xs: List. \pos: Int.
    match xs with
      nil _ -> 0
    | cons {h, t} -> if (== pos 0) then h else f t (- pos 1)
    end
);

cal_constraint = fix (\f: List -> List -> Int -> Bool. \xs: List. \pos: List. \pre: Int.
    match pos with
      nil _ -> true
    | cons {h, t} -> 
        let now = access xs h in
        and (f xs t now) (< pre now)
    end
);

get_best = \xs: List. fix (\f: LList -> Int. \l: LList.
    match l with
      lnil _ -> 0
    | lcons {h, t} -> 
        let res = f t in
        if (cal_constraint xs h -100) then (max res (sum h)) else res
    end
);

spec = \xs: List.
    let len = length xs in
    get_best xs (gen len);

main = \xs: List. spec xs;

