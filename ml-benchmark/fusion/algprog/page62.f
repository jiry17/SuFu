Inductive List = nil Unit | cons {Int, List};
Config NonLinear = true;

incre = \x: Int.
  let w = / x 10 in {w, - x (* 10 w)};

Number = {Int, List};

double = \n: Number.
  let current = fix (
    \f: List -> {Int, List}. \xs: List.
    match xs with
      nil _ -> {0, nil unit}
    | cons {h, t} ->
      let subres = f t in
        let info = incre (+ h (+ h subres.1)) in
          {info.1, cons {info.2, subres.2}}
    end
  ) in let info = current n.2 in
    {+ n.1 (+ n.1 info.1), info.2};

dnum = 10;
ratio = (fix (
  \f: Int -> Int. \n: Int.
  if == n 0 then 1 else * 2 (f (- n 1))
)) dnum;

head = \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> h
end;

multi = \xs: List. fix (
  \f: Int -> Number. \n: Int.
  if <= n 0 then {0, xs}
  else double (f (- n 1))
) dnum;

round = \xs: List.
 let res = multi xs in
  if >= (head res.2) 5 then + 1 (res.1)
  else res.1;

repr = fix (
  \f: List -> Compress List. \xs: List.
  match xs with
    nil _ -> nil unit
  | cons {h, t} -> cons {h, f t}
  end
);

val = fix (
  \f: List -> Bool. \xs: List.
  match xs with
    nil _ -> true
  | cons {h, t} ->
    if or (< h 0) (>= h 10) then false
    else f t
  end
);

main = \xs: List. if val xs then round (repr xs) else 0;


