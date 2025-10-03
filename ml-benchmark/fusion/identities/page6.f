Config VerifyBase = 300;

Inductive List = nil Unit | cons {Int, List};
Inductive List2D = nil2D Unit | cons2D {List, List2D};
Inductive List3D = nil3D Unit | cons3D {List2D, List3D};

head = \xs: List2D.
  match xs with
    nil2D _ -> nil unit
  | cons2D {h, t} -> h
  end;

tails = fix (
  \f: List -> Compress List2D. \xs: List.
  match xs with
    nil _ -> cons2D {xs, nil2D unit}
  | cons {h, t} -> cons2D {xs, f t}
  end
);

append = \w: Int. fix (
  \f: List -> List. \xs: List.
  match xs with
    nil _ -> cons {w, nil unit}
  | cons {h, t} -> cons {h, f t}
  end
);

scanl = \f: Int -> List -> List. \init: List. (fix (
  \g: List -> List -> List2D. \now: List. \xs: List.
  match xs with
    nil _ -> cons2D {now, nil2D unit}
  | cons {h, t} -> cons2D {now, g (f h now) t}
  end
)) init;

inits = scanl append (nil unit);

map = \g: List -> List2D. fix (
  \f: List2D -> List3D. \xs: List2D.
  match xs with
    nil2D _ -> nil3D unit
  | cons2D {h, t} -> cons3D {g h, f t}
  end
);

concat = fix (
  \f: List3D -> List2D. \xs: List3D.
  match xs with
    nil3D _ -> nil2D unit
  | cons3D {h, t} ->
    (fix (
      \g: List2D -> List2D. \ys: List2D.
      match ys with
        nil2D _ -> f t
      | cons2D {h1, t1} -> cons2D {h1, g t1}
      end
    )) h
  end
);

segs = \xs: List. concat (map inits (tails xs));

mapL = \g: List -> Int. fix (
  \f: List2D -> List. \xs: List2D.
  match xs with
    nil2D _ -> nil unit
  | cons2D {h, t} -> cons {g h, f t}
  end
);

sum = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> + h (f t)
  end
);

max = \a: Int. \b: Int. if < a b then b else a;
maximum = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> max h (f t)
  end
);

maxsum = \xs: List2D. maximum (mapL sum xs);

mss = \xs: List. maxsum (segs xs);