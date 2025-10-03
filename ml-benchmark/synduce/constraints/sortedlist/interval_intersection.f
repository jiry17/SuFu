Inductive List = elt {Int, Int} | cons {Int, Int, List};

head = \xs: List.
  match xs with
    elt {a, b} -> {a, b}
  | cons {a, b, t} -> {a, b}
  end;

sorted = fix (
  \f: List -> Bool. \xs: List.
  match xs with
    elt {a, b} -> < a b
  | cons {a, b, t} ->
    and (< a b) (and (< a (head t).1) (f t))
  end
);

inter = \a: Int. \b: Int. fix (
  \f: List -> Bool. \xs: List.
  match xs with
    elt {c, d} -> and (not (< b c)) (not (> a d))
  | cons {c, d, t} -> or (f t) (and (not (< b c)) (not (> a d)))
  end
);

spec = \xs: List. (fix (
  \f: List -> {Bool, Int, Int}. \xs: List.
  match xs with
    elt {a, b} -> {false, a, b}
  | cons {a, b, t} ->
    let res = f t in
      {or res.1 (inter a b t), a, b}
  end
) xs).1;

target = fix (
  \f: List -> Compress List. \xs: List.
  match xs with
    elt _ -> xs
  | cons {a, b, t} -> cons {a, b, f t}
  end
);

main = \xs: List. if sorted xs then spec (target xs) else false;