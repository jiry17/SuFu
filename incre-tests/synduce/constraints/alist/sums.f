Inductive List = nil Unit | cons {Int, List};
Inductive Nat = z Unit | s Nat;

is_unique =
  let key_differ = \key: Int. fix (
    \f: List -> Bool. \xs: List.
    match xs with
      nil _ -> true
    | cons {h, t} -> and (not (== h key)) (f t)
    end
  ) in fix (
    \f: List -> Bool. \xs: List.
    match xs with
      nil _ -> true
    | cons {h, t} -> and (key_differ h t) (f t)
    end
  );

@Input w: Int;

spec = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> if == h w then + h (f t) else f t
  end
);

target = fix (
  \f: List -> Compress List. \xs: List.
  match xs with
    nil _ -> nil unit
  | cons {h, t} -> if == h w then
    cons {h, t} /* Avoid the recusion of t*/
    else cons {h, f t}
  end
);

main = \xs: List. if is_unique xs then spec (target xs) else 0;
