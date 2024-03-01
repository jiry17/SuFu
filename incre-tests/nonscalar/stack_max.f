Config ExtraGrammar = "AutoLifter";
Config VerifyBase = 10000;
Inductive OpList = onil Unit | oadd {Int, OpList} | odel OpList;
Inductive List = nil Unit | cons {Int, List};

max = \a: Int. \b: Int. if < a b then b else a;

maximize = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> max h (f t)
  end
);

tails = \xs: List.
  match xs with
    nil _ -> xs
  | cons {h, t} -> t
  end;

eval = fix (
  \f: Compress List -> OpList -> List.
  \s: Compress List. \ops: OpList.
  match ops with
    onil _ -> nil unit
  | oadd {h, t} ->
    let news = align (label (cons {h, unlabel s})) in
    let res = align (maximize (unlabel news)) in
      cons {res, f news t}
  | odel t ->
    let news = align (label (tails (unlabel s))) in
    let res = align (maximize (unlabel news)) in
      cons {res, f news t}
  end
);

main = \op: OpList.
  let start = align (label (nil unit)) in
  eval start op;