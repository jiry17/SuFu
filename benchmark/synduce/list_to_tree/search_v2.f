Inductive Tree = empty Unit | node {Int, Tree, Tree};
Inductive List = nil Unit | cons {Int, List};

cat = fix (\f: List->List->List. \x: List. \y: List.
  match x with
    cons {h, t} -> cons {h, f t y}
  | nil _ -> y
  end
);

repr =
  let dec = fix (
    \f: List -> Tree -> List. \res: List. \t: Tree.
    match t with
      empty _ -> res
    | node {w, l, r} -> f (cons {w, f res r}) l
    end
  ) in fix (
    \f: Tree -> List. \t: Tree.
    match t with
      empty _ -> nil unit
    | node {w, l, r} -> dec (cons {w, f r}) l
    end
  );

target = fix (
  \f: Tree -> Compress Tree. \t: Tree.
  match t with
    empty _ -> empty unit
  | node {w, l, r} -> node {w, f l, f r}
  end
);

@Input x: Int;

spec = fix (
  \f: List -> Bool. \xs: List.
  match xs with
    nil _ -> false
  | cons {h, t} -> if == h x then true else f t
  end
);

main = \xs: Tree. spec (repr (target xs));