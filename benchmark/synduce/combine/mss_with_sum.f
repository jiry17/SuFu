Inductive List = elt Int | cons {Int, List};
Inductive IdxList = ielt Int | icons {Int, Int, IdxList};

max = \a: Int. \b: Int. if < a b then b else a;

sum = fix (
  \f: IdxList -> Int. \xs: IdxList.
  match xs with
    ielt t -> t
  | icons {hd, idx, tl} -> + hd (f tl)
  end
);

hsum = fix (
  \f: List -> Int. \xs: List.
  match xs with
    elt t -> t
  | cons {hd, tl} -> + hd (f tl)
  end
);

drop_sum_list = fix (
  \f: IdxList -> List. \xs: IdxList.
  match xs with
    ielt t -> elt t
  | icons {hd, idx, tl} -> cons {hd, f tl}
  end
);

mss = \xs: List. (fix (
  \f: List -> {Int, Int, Int}. \xs: List.
  match xs with
    elt t -> if (> t 0) then {t,t,t} else {0,0,0}
  | cons {hd, tl} ->
    let result = f tl in
    let mts_tl = result.1 in
    let mps_tl = result.2 in
    let mss_tl = result.3 in
    let sum_tl = hsum tl in
    let new_mps = max (+ hd mps_tl) 0 in
    let new_mts = max (+ hd sum_tl) mts_tl in
    {new_mts, new_mps, max new_mps mss_tl}
  end
) xs).3;

spec = \xs: List. mss xs;

repr = fix (
  \f: IdxList -> Compress List. \xs: IdxList.
  match xs with
    ielt t -> elt t
  | icons {hd, idx, tl} -> cons {hd, f tl}
  end
);

main = \xs: IdxList. spec (repr xs);