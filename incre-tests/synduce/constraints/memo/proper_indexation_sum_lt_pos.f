Inductive List = nil Unit | cons {Int, List};
Inductive IDList = inil Unit | icons {Int, Int, IDList};

length = fix (
  \f: IDList -> Int. \xs: IDList.
  match xs with
    inil _ -> 0
  | icons {_, _, t} -> + 1 (f t)
  end
);

is_indexed = fix (
  \f: IDList -> Bool. \xs: IDList.
  match xs with
    inil _ -> true
  | icons {_, id, t} -> and (f t) (== id (length t))
  end
);

repr = fix (
  \f: IDList -> List. \m: IDList.
  match m with
    inil _ -> nil unit
  | icons {h, _, t} -> cons {h, f t}
  end
);

len = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> + 1 (f t)
  end
);

spec = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> if > h (len t) then + h (f t) else f t
  end
);

target = fix (
  \f: IDList -> Compress IDList. \xs: IDList.
  match xs with
    inil _ -> xs
  | icons {h, id, t} -> icons {h, id, f t}
  end
);

main = /*\t: Tree. let mt = gen t in*/
  \m: IDList.
    if is_indexed m then spec (repr (target m)) else 0;