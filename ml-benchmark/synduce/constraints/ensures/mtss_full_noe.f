Inductive List = elt Int | cons {Int, List};
Inductive NList = line List | ncons {List, NList};
Inductive CNList = sglt List | cat {CNList, CNList};

cton = fix (
  \f: CNList -> NList.
  let dec = fix (
    \g: CNList -> CNList -> NList. \l: CNList. \c: CNList.
    match c with
      sglt x -> ncons {x, f l}
    | cat {x, y} -> g (cat {y, l}) x
    end
  ) in \c: CNList.
  match c with
    sglt x -> line x
  | cat {x, y} -> dec y x
  end
);

sum = fix (
  \f: List -> Int. \xs: List.
  match xs with
    elt x -> x
  | cons {h, t} -> + h (f t)
  end
);

max = \a: Int. \b: Int. if < a b then b else a;

spec = \xs: NList. (fix (
  \f: NList -> {Int, Int}. \xs: NList.
  match xs with
    line a -> {max 0 (sum a), sum a}
  | ncons {h, t} ->
    let hsum = sum h in
      let tres = f t in
        {max (+ tres.1 hsum) 0, + tres.2 hsum}
  end
) xs).1;

target = fix (
  \f: CNList -> Compress CNList.
  let tsum = fix (
    \g: List -> Compress List. \xs: List.
    match xs with
      elt x -> elt x
    | cons {h, t} -> cons {h, g t}
    end
  ) in \c: CNList.
  match c with
    sglt x -> sglt (tsum x)
  | cat {l, r} -> cat {f l, f r}
  end
);

main = \c: CNList. spec (cton (target c));