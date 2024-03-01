Inductive UList = unil Unit | uelt Int | usplit {UList, Int, Int, UList};
Inductive List = nil Unit | cons {Int, List};

repr = (fix (
  \f: List -> UList -> List. \res: List. \xs: UList.
  match xs with
    unil _ -> res
  | uelt x -> cons {x, res}
  | usplit {x, a, b, y} -> f (cons {a, cons {b, f res y}}) x
  end
)) (nil unit);

is_unimodal =
  let aux_down = fix (
    \f: Int -> List -> Bool. \pre: Int. \xs: List.
    match xs with
      nil _ -> > pre 0
    | cons {h, t} -> and (> pre 0) (and (> pre h) (f h t))
    end
  ) in let aux_up = fix (
    \f: Int -> List -> Bool. \pre: Int. \xs: List.
    match xs with
      nil _ -> > pre 0
    | cons {h, t} -> and (> pre 0) (if < pre h then f h t else aux_down h t)
    end
  ) in \xs: List.
  match xs with
    nil _ -> true
  | cons {h, t} -> aux_up h t
  end;

max = \a: Int. \b: Int. if < a b then b else a;

spec = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> max h (f t)
  end
);

target = fix (
  \f: UList -> Compress UList. \xs: UList.
  match xs with
    unil _ -> unil unit
  | uelt x -> uelt x
  | usplit {x, a, b, y} ->
    if > a b then usplit {f x, a, b, y} /*Avoid the recursion of y*/
    else usplit {x, a, b, f y} /*Avoid the recusion of x*/
  end
);

main = \xs: UList. if is_unimodal (repr xs) then spec (repr (target xs)) else 0;