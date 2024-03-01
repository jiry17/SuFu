Inductive List = nil Unit | cons {Int, List};
Inductive Nat = z Unit | s Nat;
Inductive AList = anil Unit | acons {Int, Nat, AList};

is_unique =
  let key_differ = \key: Int. fix (
    \f: AList -> Bool. \xs: AList.
    match xs with
      anil _ -> true
    | acons {h, _, t} -> and (not (== h key)) (f t)
    end
  ) in fix (
    \f: AList -> Bool. \xs: AList.
    match xs with
      anil _ -> true
    | acons {h, _, t} -> and (key_differ h t) (f t)
    end
  );

@Input w: Int;

spec = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> if == h w then + 1 (f t) else f t
  end
);

repr =
  let repeat = \w: Int. \suf: List. fix (
    \f: Nat -> List. \n: Nat.
    match n with
      z _ -> suf
    | s m -> cons {w, f m}
    end
  ) in fix (
    \f: AList -> List. \xs: AList.
    match xs with
      anil _ -> nil unit
    | acons {h, n, t} -> repeat h (f t) n
    end
  );

n2i = fix (
  \f: Nat -> Int. \n: Nat.
  match n with
    z _ -> 0
  | s m -> + 1 (f m)
  end
);

target = fix (
  \f: AList -> Compress AList. \xs: AList.
  match xs with
    anil _ -> anil unit
  | acons {h, n, t} -> if == h w then
    let num = n2i n in /*This invocation is provided in Synduce's template*/
    acons {h, n, t} /* Avoid the recursion of t*/
    else acons {h, n, f t}
  end
);

main = \xs: AList. if is_unique xs then spec (repr (target xs)) else 0;
