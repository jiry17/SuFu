Inductive List = elt Int | cons {Int, List};

head = \xs: List. match xs with elt w -> w | cons {h, t} -> h end;

is_const = fix (
  \f: List -> Bool. \xs: List.
  match xs with
    elt x -> true
  | cons {h, t} -> and (== h (head t)) (f t)
  end
);

@Input w: Int;

spec = fix (
  \f: List -> Int. \xs: List.
  match xs with
    elt x -> if == w x then 1 else 0
  | cons {h, t} -> if == w h then 1 else if == 0 (f t) then 0 else + 1 (f t)
  end
);


target = fix (
  \f: List -> Compress List. \xs: List.
  match xs with
    elt a -> elt a
  | cons {h, t} -> cons {h, t} /*Avoid the recursion on t*/
  end
);

main = \xs: List. if is_const xs then spec (target xs) else 0;
