Inductive List = elt {Int, Int} | cons {Int, Int, List};
Inductive CList = single {Int, Int} | concat {CList, CList};

cat = fix (
  \f: List -> List -> List. \a: List. \b: List.
  match a with
    elt {x, y} -> cons {x, y, b}
  | cons {x, y, t} -> cons {x, y, (f t b)}
  end
);

repr = fix (
    \f: CList -> List. \xs: CList.
    match xs with
      single {a, b} -> elt {a, b}
    | concat {a, b} -> cat (f a) (f b)
    end
);

is_sorted =
  let aux = fix (
    \f: Int -> List -> Bool. \pre: Int. \xs: List.
    match xs with
      elt {a, b} -> <= pre (+ a b)
    | cons {a, b, t} -> and (<= pre (+ a b)) (f (+ a b) t)
    end
  ) in \xs: List.
  match xs with
    elt _ -> true
  | cons {a, b, t} -> aux (+ a b) t
  end;

c_sorted = \xs: CList. is_sorted (repr xs);

max = \x: Int. \y: Int. if < x y then y else x;

spec = fix (
  \f: List -> Int. \xs: List.
  match xs with
    elt {a, b} -> + a b
  | cons {a, b, t} -> max (f t) (+ a b)
  end
);

target = fix (
  \f: CList -> Compress CList. \xs: CList.
  match xs with
    single {a, b} -> xs
  | concat {l, r} -> concat {l, f r} /*Avoid the recursion of l*/
  end
);

main = \xs: CList. if c_sorted xs then spec (repr (target xs)) else 0;
