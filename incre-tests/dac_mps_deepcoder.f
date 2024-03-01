/*Library for lists*/

Inductive List = cons {Int, List} | nil Unit;

fold = \oplus: Int -> Int -> Int. \e: Int.
  fix (\f: List->Int. \xs: List.
    match xs with
      cons {h, t} -> oplus h (f t)
    | nil _ -> e
    end
  );

error = 100;
@Combine inf = 100;

@Align plus = \a: Int. \b: Int. + a b;
@Align minus = \a: Int. \b: Int. - a b;
@Align times = \a: Int. \b: Int. * a b;
@Align min = \a: Int. \b: Int. if (< a b) then a else b;
@Align max = \a: Int. \b: Int. if (> a b) then a else b;
@Align sum = fold plus 0;
@Align length = fold (\a: Int. \b: Int. + b 1) 0;
@Align head = \xs: List. match xs with nil _ -> error | cons {h, _} -> h end;
@Align neg = \a: Int. - 0 a;

@Align last = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> error
  | cons {h, nil _} -> h
  | cons {h, t} -> f t
  end
);

@Align access = \pos: Int. \xs: List.
  let len = length xs in
    let ind = if (< pos 0) then + pos len else pos in
      if or (< ind 0) (>= ind len) then error
      else (fix (
        \f: Int -> List -> Int. \i: Int. \ys: List.
        match ys with
          cons {h, t} -> if (== i 0) then h else f (- i 1) t
        end
      )) ind xs;

@Align count = \p: Int -> Bool. fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> if p h then (+ 1 (f t)) else f t
  end
);

@Align maximal = fold max (- 0 inf);
@Align minimal = fold min inf;

@Align take = \pos: Int. \xs: List.
  let len = length xs in
    let ind = if < pos 0 then + pos len else pos in
      (fix (\f: Int -> List -> List. \i: Int. \ys: List.
        match ys with
          nil _ -> ys
        | cons {h, t} -> if < i 0 then nil unit else cons {h, f (- i 1) t}
        end
      )) ind xs;

@Align drop = \pos: Int. \xs: List.
  let len = length xs in
    let ind = if < pos 0 then + pos len else pos in
      (fix (\f: Int -> List -> List. \i: Int. \ys: List.
        match ys with
          nil _ -> ys
        | cons {h, t} -> if (>= i ind) then cons {h, f (+ i 1) t} else f (+ i 1) t
        end
      )) 0 xs;

@Align rev = fix (
  \f: List -> List -> List. \res: List. \xs: List.
  match xs with
    nil _ -> res
  | cons {h, t} -> f (cons {h, res}) t
  end
) (nil unit);

@Align map = \op: Int -> Int. fix (
  \f: List -> List. \xs: List.
  match xs with
    nil _ -> xs
  | cons {h, t} -> cons {op h, f t}
  end
);


@Align filter = \p: Int -> Bool. fix (
  \f: List -> List. \xs: List.
  match xs with
    nil _ -> xs
  | cons {h, t} -> if (p h) then cons {h, f t} else f t
  end
);

@Align zip = \op: Int -> Int -> Int. fix (
  \f: List -> List -> List. \xs: List. \ys: List.
  match xs with
    nil _ -> nil unit
  | cons {h1, t1} ->
    match ys with
      nil _ -> nil unit
    | cons {h2, t2} -> cons {op h1 h2, f t1 t2}
    end
  end
);

concat = \xs: List. \ys: List. (fix (
  \f: List -> List. \zs: List.
  match zs with
    nil _ -> ys
  | cons {h, t} -> cons {h, f t}
  end
)) xs;

@Align sort = fix (
  \f: List -> List. \xs: List.
  match xs with
    nil _ -> xs
  | cons {h, t} ->
    let l = filter (\a: Int. < a h) t in
      let r = filter (\a: Int. <= h a) t in
        concat (f l) (cons {h, (f r)})
  end
);

@Align scanl = \oplus: Int -> Int -> Int. \xs: List.
  match xs with
    nil _ -> xs
  | cons {h, t} ->
    let rec = fix (\f: Int -> List -> List. \pre: Int. \xs: List.
      match xs with
        cons {h, t} ->
          let now = oplus pre h in
            cons {now, f now t}
      | nil _ -> xs
      end
    ) in
      cons {h, rec h t}
  end;

@Align scanr = \oplus: Int -> Int -> Int. fix (
  \f: List -> List. \xs: List.
  match xs with
    nil _ -> xs
  | cons {h, nil _} -> xs
  | cons {h, t} ->
    let res = f t in
      cons {oplus h (head res), res}
  end
);

@Align isneg = \a: Int. < a 0;
@Align ispos = \a: Int. > a 0;
@Align iseven = \a: Int. == a (* (/ a 2) 2);
@Align isodd = \a: Int. not (iseven a);
/*Sketches for DAC*/

dac = fix (lambda f: List -> Int -> {List, Compress List}. lambda x: List. lambda len: Int.
  if (== len 1) then
    match x with
      cons {h, t} -> {t, cons {h, nil unit}}
    end
  else let llen = (/ len 2) in
    let lres = f x llen in
      let rres = f lres.1 (- len llen) in
        {rres.1, concat lres.2 rres.2}
);

max = lambda x: Int. lambda y: Int. if (> x y) then x else y;

@Align mps = fix (lambda f: List -> Int. lambda l: List.
  match l with
    nil _ -> 0
  | cons {h, t} ->
    let res = (f t) in
      max 0 (+ res h)
  end
);

compress_mps = lambda x: List. mps (dac x (length x)).2 ;

/*
l = cons {2, cons {-1, cons {3, nil unit}}};
compress_mps l;*/