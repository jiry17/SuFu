Inductive List = elt Int | cons {Int, List};
Inductive Pos = one Unit | s Pos;
Inductive AList = aelt {Int, Pos} | acons {Int, Pos, AList};

is_unique =
  let key_differ = \key: Int. fix (
    \f: AList -> Bool. \xs: AList.
    match xs with
      aelt {h, _} -> not (== h key)
    | acons {h, _, t} -> and (not (== h key)) (f t)
    end
  ) in fix (
    \f: AList -> Bool. \xs: AList.
    match xs with
      aelt _ -> true
    | acons {h, _, t} -> and (key_differ h t) (f t)
    end
  );

max = \a: Int. \b: Int. if < a b then b else a;
count = \w: Int. fix (
  \f: List -> Int. \xs: List.
  match xs with
    elt h -> if == h w then 1 else 0
  | cons {h, t} -> + (if == h w then 1 else 0) (f t)
  end
);

spec = \xs: List. (fix (
  \f: List -> {Int, Int}. \xs: List.
  match xs with
    elt h -> {1, h}
  | cons {h, t} ->
    let res = f t in
      let c = count h xs in
        if > c res.1 then {c, h} else res
  end
) xs).2;

repr =
  let repeat = \w: Int. \suf: List. fix (
    \f: Pos -> List. \n: Pos.
    match n with
      one _ -> cons {w, suf}
    | s m -> cons {w, f m}
    end
  ) in let dup = \w: Int. fix (
    \f: Pos -> List. \n: Pos.
    match n with
      one _ -> elt w
    | s m -> cons {w, f m}
    end
  ) in fix (
    \f: AList -> List. \xs: AList.
    match xs with
      aelt {h, n} -> dup h n
    | acons {h, n, t} -> repeat h (f t) n
    end
  );

p2i = fix (
  \f: Pos -> Int. \n: Pos.
  match n with
    one _ -> 1
  | s m -> + 1 (f m)
  end
);

target = fix (
  \f: AList -> Compress AList. \xs: AList.
  match xs with
    aelt {h, n} ->
    let num = p2i n in /*This invocation is provided in Synduce's template*/
      xs
  | acons {h, n, t} ->
    let num = p2i n in /*This invocation is provided in Synduce's template*/
      acons {h, n, f t}
  end
);

main = \xs: AList. if is_unique xs then spec (repr (target xs)) else 0;
