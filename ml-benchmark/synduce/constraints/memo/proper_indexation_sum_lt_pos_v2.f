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

max = \a: Int. \b: Int. if > a b then a else b;

spec = fix (
  \f: List -> {Int, Int}. \xs: List.
  match xs with
    nil _ -> {0, 0}
  | cons {h, t} ->
    let res = f t in
      {if > h res.2 then max (+ res.1 h) 0 else res.1, + res.2 1}
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
    if is_indexed m then spec (repr (target m)) else {0, 0};