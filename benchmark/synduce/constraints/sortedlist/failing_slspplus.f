Inductive List = elt Int | cons {Int, List};
Inductive NList = line List | ncons {List, NList};
Inductive CList = sglt List | cat {CList, Int, CList};

c2n =
  let aux = fix (
    \f: NList -> CList -> NList. \pre: NList. \c: CList.
    match c with
      sglt a -> ncons {a, pre}
    | cat {l, _, r} -> f (f pre r) l
    end
  ) in fix (
    \f: CList -> NList. \c: CList.
    match c with
      sglt a -> line a
    | cat {l, _, r} -> aux (f r) l
    end
  );

lsum = fix (
  \f: List -> Int. \xs: List.
  match xs with
    elt x -> x
  | cons {h, t} -> + h (f t)
  end
);

min = \a: Int. \b: Int. if < a b then a else b;
max = \a: Int. \b: Int. if < a b then b else a;

sorted =
  let lmin = fix (
    \f: CList -> Int. \c: CList.
    match c with
      sglt a -> lsum a
    | cat {l, _, r} -> min (f l) (f r)
    end
  ) in let lmax = fix (
    \f: CList -> Int. \c: CList.
    match c with
      sglt a -> lsum a
    | cat {l, _, r} -> max (f l) (f r)
    end
  ) in fix (
  \f: CList -> Bool. \c: CList.
  match c with
    sglt a -> true
  | cat {l, piv, r} -> and (and (< (lmax l) piv) (< piv (lmin r))) (and (f l) (f r))
  end
);

spec = \xs: NList. (fix (
  \f: NList -> {Int, Bool}. \xs: NList.
  match xs with
    line a -> {max 0 (lsum a), >= (lsum a) 0}
  | ncons {h, t} ->
    let res = f t in
      let line_sum = lsum h in
        {
        if and res.2 (>= line_sum 0) then + res.1 line_sum else res.1,
        and res.2 (>= line_sum 0)
        }
  end
) xs).1;

target =
  let list_repr = fix (
    \f: List -> Compress List. \xs: List.
    match xs with
      elt a -> xs
    | cons {h, t} -> cons {h, f t}
    end
  ) in fix (
    \f: CList -> Compress CList. \c: CList.
    match c with
      sglt x -> sglt (list_repr x)
    | cat {l, piv, r} ->
      if <= piv 0 then cat {l, piv, f r} /*Avoid the recursion of l*/
      else cat {f l, piv, f r}
    end
  );

main = \c: CList. if sorted c then spec (c2n (target c)) else 0;