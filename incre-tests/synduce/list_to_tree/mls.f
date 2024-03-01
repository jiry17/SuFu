Inductive Tree = empty Unit | node {Int, Tree, Tree};
Inductive List = nil Unit | cons {Int, List};

max = \a: Int. \b: Int. if (< a b) then b else a;

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
    | node {w, l, r} -> cons {w, f (f res l) r}
    end
  ) in fix (
    \f: Tree -> List. \t: Tree.
    match t with
      empty _ -> nil unit
    | node {w, l, r} -> cons {w, dec (f l) r}
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
  \f: List -> {Int, Int}. \xs: List.
  match xs with
    nil _ -> {0,0}
  | cons {h, t} -> 
    let result = f t in
    {+ h result.1, max result.2 (+ h result.1)}
  end
);

main = \xs: Tree. spec (repr (target xs));