Inductive UList = uelt Int | usplit {UList, Int, Int, UList};
Inductive List = elt Int | cons {Int, List};

repr =
let aux = (fix (
  \f: List -> UList -> List. \res: List. \xs: UList.
  match xs with
    uelt x -> cons {x, res}
  | usplit {x, a, b, y} -> f (cons {a, cons {b, f res y}}) x
  end
)) in fix (
  \f: UList -> List. \xs: UList.
  match xs with
    uelt x -> elt x
  | usplit {x, a, b, y} -> aux (cons {a, cons {b, f y}}) x
  end
);

is_unimodal =
  let aux_down = fix (
    \f: Int -> List -> Bool. \pre: Int. \xs: List.
    match xs with
      elt x -> > pre x
    | cons {h, t} -> and (> pre h) (f h t)
    end
  ) in let aux_up = fix (
    \f: Int -> List -> Bool. \pre: Int. \xs: List.
    match xs with
      elt x -> < pre x
    | cons {h, t} -> if < pre h then f h t else aux_down h t
    end
  ) in \xs: List.
  match xs with
    elt x -> true
  | cons {h, t} -> aux_up h t
  end;

max = \a: Int. \b: Int. if < a b then b else a;

spec = fix (
  \f: List -> Int. \xs: List.
  match xs with
    elt x -> x
  | cons {h, t} -> max h (f t)
  end
);

target = fix (
  \f: UList -> Compress UList. \xs: UList.
  match xs with
    uelt x -> uelt x
  | usplit {x, a, b, y} ->
    if > a b then usplit {f x, a, b, y} /*Avoid the recursion of y*/
    else usplit {x, a, b, f y} /*Avoid the recusion of x*/
  end
);

main = \xs: UList. if is_unimodal (repr xs) then spec (repr (target xs)) else 0;