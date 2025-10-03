Inductive List = elt Int | cons {Int, List};
Inductive CList = single Int | concat {Int, CList, CList};

cat = fix (
  \f: List -> List -> List. \a: List. \b: List.
  match a with
    elt w -> cons {w, b}
  | cons {h, t} -> cons {h, (f t b)}
  end
);

repr = fix (
  \f: CList -> List. \c: CList.
  match c with
    single w -> elt w
  | concat {w, l, r} -> cat (f l) (f r)
  end
);

max = \a: Int. \b: Int. if < a b then b else a;
min = \a: Int. \b: Int. if < a b then a else b;

lmax = fix (
  \f: CList -> Int. \c: CList.
  match c with
    single w -> w
  | concat {w, l, r} -> max (f l) (f r)
  end
);

lmin = fix (
  \f: CList -> Int. \c: CList.
  match c with
    single w -> w
  | concat {w, l, r} -> min (f l) (f r)
  end
);

is_parti = fix (
  \f: CList -> Bool. \c: CList.
  match c with
    single w -> true
  | concat {w, l, r} -> and (and (< (lmax l) w) (< w (lmin r))) (and (f l) (f r))
  end
);

spec = fix (
  \f: List -> {Int, Int, Int, Int}. \xs: List.
  match xs with
    elt w -> {w, max w 0, max w 0, max w 0}
  | cons {h, t} ->
    let res = f t in
      {+ res.1 h, max res.2 (+ res.1 h), max (+ res.3 h) 0, max res.4 (+ res.3 h)}
  end
);

sum = fix (
  \f: CList -> Int. \c: CList.
  match c with
    single a -> a
  | concat {a, l, r} -> + (f l) (f r)
  end
);

target = fix (
  \f: CList -> Compress CList. \xs: CList.
  match xs with
    single a -> xs
  | concat {w, l, r} ->
    if < w 0 then
      let s = sum l in /*This invocation is provided in Synduce's template*/
        concat {w, l, f r} /*Avoid the recursion of l*/
    else concat {w, f l, f r}
  end
);

/*Customized Generator*/

insert = \w: Int. fix (
  \f: List -> List. \xs: List.
  match xs with
    elt a -> if < w a then cons {w, elt a} else cons {a, elt w}
  | cons {h, t} ->
    if < w h then cons {w, xs} else cons {h, f t}
  end
);

sort = fix (
  \f: List -> List. \xs: List.
  match xs with
    elt w -> xs
  | cons {h, t} -> insert h (f t)
  end
);

access = \x: List. match x with
  elt w -> {w, x}
| cons {h, t} -> {h, t}
end;

fill = fix (
  \f: CList -> List -> {CList, List}. \c: CList. \xs: List.
  match c with
    single _ ->
      let info = access xs in
        {single info.1, info.2}
  | concat {_, l, r} ->
    let lres = f l xs in
      let info = access lres.2 in
        let rres = f r info.2 in
          {concat {info.1, lres.1, rres.1}, rres.2}
  end
);

flatten = fix (
  \f: CList -> List. \c: CList.
  match c with
    single w -> elt w
  | concat {w, l, r} -> cat (f l) (cons {w, (f r)})
  end
);

gen = \c: CList. let xs = sort (flatten c) in
  (fill c xs).1;

main = \xs: CList.
  let inp = gen xs in
    if is_parti inp then spec (repr (target inp)) else {0, 0, 0, 0};