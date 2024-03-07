Inductive List = elt Int | cons {Int, List};
Inductive IDList = ielt Int | icons {Int, Int, IDList};

is_sorted =
  let aux = fix (
    \f: Int -> IDList -> Bool. \pre: Int. \xs: IDList.
    match xs with
      ielt x -> > pre x
    | icons {h, _, t} -> and (> pre h) (f h t)
    end
  ) in \xs: IDList.
  match xs with
    ielt x -> true
  | icons {h, _, t} -> aux h t
  end;

len = fix (
  \f: IDList -> Int. \xs: IDList.
  match xs with
    ielt _ -> 1
  | icons {_, _, t} -> + 1 (f t)
  end
);

len_raw = fix (
  \f: List -> Int. \xs: List.
  match xs with
    elt _ -> 1
  | cons {_, t} -> + 1 (f t)
  end
);

is_indexed = fix (
  \f: IDList -> Bool. \xs: IDList.
  match xs with
    ielt _ -> true
  | icons {_, id, t} -> and (== id (len xs)) (f t)
  end
);

add_index = fix (
  \f: List -> IDList. \xs: List.
  match xs with
    elt a -> ielt a
  | cons {h, t} -> icons {h, len_raw xs, f t}
  end
);

drop_index = fix (
  \f: IDList -> List. \xs: IDList.
  match xs with
    ielt a -> elt a
  | icons {h, _, t} -> cons {h, f t}
  end
);

@Input w: Int;

spec = fix (
  \f: List -> Int. \xs: List.
  match xs with
    elt x -> if < x w then 1 else 0
  | cons {h, t} -> + (f t) (if < h w then 1 else -1)
  end
);

target = fix (
  \f: IDList -> Compress IDList. \xs: IDList.
  match xs with
    ielt x -> xs
  | icons {h, idx, t} -> if < h w then xs else icons {h, idx, f t}
  end
);

/*Customized generator*/
main = \xs: List. let inp = add_index xs in
  if and (is_sorted inp) (is_indexed inp) then spec (drop_index (target inp)) else 0;