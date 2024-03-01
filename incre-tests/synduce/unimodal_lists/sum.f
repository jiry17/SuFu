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
      nil _ -> true
    | cons {h, t} -> and (>= pre h) (f h t)
    end
  ) in let aux_up = fix (
    \f: Int -> List -> Bool. \pre: Int. \xs: List.
    match xs with
      nil _ -> true
    | cons {h, t} -> if <= pre h then f h t else aux_down h t
    end
  ) in \xs: List.
  match xs with
    nil _ -> true
  | cons {h, t} -> aux_up h t
  end;

spec = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> + h (f t)
  end
);

target = fix (
  \f: UList -> Compress UList. \xs: UList.
  match xs with
    unil _ -> unil unit
  | uelt x -> uelt x
  | usplit {x, a, b, y} -> usplit {f x, a, b, f y}
  end
);

main = \xs: UList. if is_unimodal (repr xs) then spec (repr (target xs)) else 0;