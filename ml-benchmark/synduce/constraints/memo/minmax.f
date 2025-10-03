Inductive Tree =
  leaf Int | node {Int, Int, Int, Int, Int, Tree, Tree};

min = \a: Int. \b: Int. if < a b then a else b;
max = \a: Int. \b: Int. if < a b then b else a;

tmin = fix (
  \f: Tree -> Int. \t: Tree.
  match t with
    leaf w -> w
  | node {_, _, _, _, a, l, r} -> min a (min (f l) (f r))
  end
);

tmax = fix (
  \f: Tree -> Int. \t: Tree.
  match t with
    leaf w -> w
  | node {_, _, _, _, a, l, r} -> max a (max (f l) (f r))
  end
);

is_memo = fix (
  \f: Tree -> Bool. \t: Tree.
  match t with
    leaf _ -> true
  | node {lmin, lmax, rmin, rmax, v, l, r} ->
    and (and (and (== lmin (tmin l)) (== lmax (tmax l))) (and (== rmin (tmin r)) (== rmax (tmax r))))
    (and (f l) (f r))
  end
);

spec = \t: Tree. {tmin t, tmax t};

target = fix (
  \f: Tree -> Compress Tree. \t: Tree.
  match t with
    leaf x -> t
  | node {lmin, lmax, rmin, rmax, v, _, _} -> t /*Avoid recursions*/
  end
);

/*Customized generator*/
gen = fix (
  \f: Tree -> Tree. \t: Tree.
  match t with
    leaf x -> leaf x
  | node {_, _, _, _, v, l, r} ->
    node {tmin l, tmax l, tmin r, tmax r, v, f l, f r}
  end
);

main = \t: Tree.
  let inp = gen t in
    if is_memo inp then spec (target inp) else {0, 0};