Inductive List = cons {Int, List} | nil Unit;

al_fold = \oplus: Int -> Int -> Int. \e: Int.
  fix (\f: List->Int. \xs: List.
    match xs with
      cons {h, t} -> oplus h (f t)
    | nil _ -> e
    end
  );

al_error = 100;
@Combine al_inf = 100;

@Align @NoPartial al_plus = \a: Int. \b: Int. + a b;
@Align @NoPartial al_minus = \a: Int. \b: Int. - a b;
@Align @NoPartial al_times = \a: Int. \b: Int. * a b;
@Align @NoPartial al_min = \a: Int. \b: Int. if (< a b) then a else b;
@Align @NoPartial al_max = \a: Int. \b: Int. if (> a b) then a else b;
@Align al_maximum = al_fold al_max (- 0 al_inf);
@Align al_minimum = al_fold al_min al_inf;
@Align al_sum = al_fold al_plus 0;
@Align al_length = al_fold (\a: Int. \b: Int. + b 1) 0;
@Align @Extract al_head = \xs: List. match xs with nil _ -> al_inf | cons {h, _} -> h end;
@Align al_inc = \a: Int. + a 1;
@Align al_dec = \a: Int. - a 1;
@Align al_neg = \a: Int. - 0 a;

@Align al_last = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> al_error
  | cons {h, nil _} -> h
  | cons {h, t} -> f t
  end
);

@Align @NoPartial al_access = \pos: Int. \xs: List.
  let len = al_length xs in
    let ind = if (< pos 0) then + pos len else pos in
      if or (< ind 0) (>= ind len) then al_error
      else (fix (
        \f: Int -> List -> Int. \i: Int. \ys: List.
        match ys with
          cons {h, t} -> if (== i 0) then h else f (- i 1) t
        end
      )) ind xs;

@Align @NoPartial al_count = \p: Int -> Bool. fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> if p h then (+ 1 (f t)) else f t
  end
);

@Align @NoPartial al_take = \pos: Int. \xs: List.
  let len = al_length xs in
    let ind = if < pos 0 then + pos len else pos in
      (fix (\f: Int -> List -> List. \i: Int. \ys: List.
        match ys with
          nil _ -> ys
        | cons {h, t} -> if < i 0 then nil unit else cons {h, f (- i 1) t}
        end
      )) ind xs;

@Align @NoPartial al_drop = \pos: Int. \xs: List.
  let len = al_length xs in
    let ind = if < pos 0 then + pos len else pos in
      (fix (\f: Int -> List -> List. \i: Int. \ys: List.
        match ys with
          nil _ -> ys
        | cons {h, t} -> if (>= i ind) then cons {h, f (+ i 1) t} else f (+ i 1) t
        end
      )) 0 xs;

@Align al_rev = fix (
  \f: List -> List -> List. \res: List. \xs: List.
  match xs with
    nil _ -> res
  | cons {h, t} -> f (cons {h, res}) t
  end
) (nil unit);

@Align @NoPartial al_map = \op: Int -> Int. fix (
  \f: List -> List. \xs: List.
  match xs with
    nil _ -> xs
  | cons {h, t} -> cons {op h, f t}
  end
);

@Align @NoPartial al_filter = \p: Int -> Bool. fix (
  \f: List -> List. \xs: List.
  match xs with
    nil _ -> xs
  | cons {h, t} -> if (p h) then cons {h, f t} else f t
  end
);

@Align @NoPartial al_zip = \op: Int -> Int -> Int. fix (
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

al_concat = \xs: List. \ys: List. (fix (
  \f: List -> List. \zs: List.
  match zs with
    nil _ -> ys
  | cons {h, t} -> cons {h, f t}
  end
)) xs;

@Align al_sort = fix (
  \f: List -> List. \xs: List.
  match xs with
    nil _ -> xs
  | cons {h, t} ->
    let l = al_filter (\a: Int. < a h) t in
      let r = al_filter (\a: Int. <= h a) t in
        al_concat (f l) (cons {h, (f r)})
  end
);

@Align @NoPartial al_scanl = \oplus: Int -> Int -> Int. \xs: List.
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

@Align @NoPartial al_scanr = \oplus: Int -> Int -> Int. fix (
  \f: List -> List. \xs: List.
  match xs with
    nil _ -> xs
  | cons {h, nil _} -> xs
  | cons {h, t} ->
    let res = f t in
      cons {oplus h (al_head res), res}
  end
);

@Align al_isneg = \a: Int. < a 0;
@Align al_ispos = \a: Int. > a 0;
@Align al_iseven = \a: Int. == a (* (/ a 2) 2);
@Align al_isodd = \a: Int. not (al_iseven a);
@Align one = 1;
@Align none = -1;
/*@Combine al_error = \x: Int. or (== x al_error) (== x (- 0 al_error));*/