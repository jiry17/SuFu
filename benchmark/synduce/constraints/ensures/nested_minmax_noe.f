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


max = \a: Int. \b: Int. if < a b then b else a;
min = \a: Int. \b: Int. if < a b then a else b;

range = fix (
  \f: List -> {Int, Int}. \xs: List.
  match xs with
    elt w -> {w, w}
  | cons {h, t} ->
    let res = f t in
      {min h res.1, max h res.2}
  end
);

spec = fix (
  \f: NList -> {Int, Int}. \xs: NList.
  match xs with
    line a -> range a
  | ncons {h, t} ->
    let rh = range h in
      let res = f t in
        {min rh.1 res.1, max rh.2 res.2}
  end
);

target = fix (
  \f: CNList -> Compress CNList. \c: CNList.
  match c with
    sglt x ->
    let info = range x in /*This invocation is provided in Synduce's template*/
      c
  | cat {l, r} -> cat {f l, f r}
  end
);

main = \c: CNList. spec (cton (target c));