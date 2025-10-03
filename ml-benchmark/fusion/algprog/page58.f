Config SampleIntMin = -2;
Config SampleIntMax = 2;
Config NonLinear = true;
Inductive List = nil Unit | cons {Int, List};

tri = \op: Int -> Int. fix (
  \f: List -> Compress List. \xs: List.
  match xs with
    nil _ -> nil unit
  | cons {h, t} ->
    let tail = (fix (
      \g: List -> List. \ys: List.
      match ys with
        nil _ -> nil unit
      | cons {h, t} -> cons {op h, g t}
      end
    )) (f t) in cons {h, tail}
  end
);

@Input w: Int;
op = \x: Int. * x w;

sum = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> + h (f t)
  end
);

main = \xs: List. sum (tri op xs);