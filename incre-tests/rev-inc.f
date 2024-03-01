/*Library for lists*/

Inductive List = cons {Int, List} | nil Unit;

inc = fix (
  \f: List -> List. \xs: List.
  match xs with
    nil _ -> nil unit
  | cons {h, t} -> cons {+ 1 h, f t}
  end
);

rev = fix (
  \f: Compress List -> List -> Compress List. \res: Compress List. \xs: List.
  match xs with
    nil _ -> res
  | cons {h, t} -> f (align (label (cons {h, unlabel res}))) t
  end
) (align (label (nil unit)));

main = \xs: List. let tmp = rev xs in align (inc (unlabel tmp));