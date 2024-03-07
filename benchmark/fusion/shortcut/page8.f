Inductive List = nil Unit | cons {Int, List};
Inductive NList = nnil Unit | ncons {List, NList};

length = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> + 1 (f t)
  end
);

sum = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> + h (f t)
  end
);

append = \w: Int. fix (
  \f: List -> List. \a: List.
  match a with
    nil _ -> cons {w, nil unit}
  | cons {h, t} -> cons {h, (f t)}
  end
);

cat = fix (
  \f: List -> Compress List -> Compress List. \a: List. \b: Compress List.
  match a with
    nil _ -> b
  | cons {h, t} -> cons {h, (f t b)}
  end
);

concat = fix (
  \f: NList -> Compress List. \xs: NList.
  match xs with
    nnil _ -> nil unit
  | ncons {h, t} -> cat h (f t)
  end
);

safe = \p: List. \l: Compress List. \n: Int.
  let m = + 1 (length l) in
    fix (
      \f: List -> Int -> Bool. \xs: List. \i: Int.
      match xs with
        nil _ -> true
      | cons {j, t} ->
        if or (== j n) (or (== (+ i j) (+ n m)) (== (- i j) (- m n))) then false
        else f t (+ i 1)
      end
    ) p 1;

queens = \n: Int. (fix (
  \f: Int -> NList. \m: Int.
  if == m 0 then ncons {nil unit, nnil unit}
  else let subres = f (- m 1) in
    let enum = fix (
      \g: NList -> Int -> NList. \sols: NList. \choice: Int.
      match sols with
        nnil _ -> if == choice n then nnil unit else g subres (+ choice 1)
      | ncons {sol, remain} ->
        let tailres = g remain choice in
          if safe sol sol choice then
            ncons {append choice sol, tailres}
          else tailres
      end
    ) in (enum subres 1)
)) n;

@Start main = \n: Int. if > n 0 then queens n else nnil unit;