Inductive List = nil Unit | cons {Int, List};
Inductive PList = pnil Unit | pcons {List, PList};

ref = \op: List -> Int.
let update = \pre: List. fix (
  \f: List -> {List, List}. \xs: List.
  match xs with
    nil _ -> {nil unit, pre}
  | cons {h, t} ->
    let res = f t in
    let now = cons {h, res.2} in
    {cons {op now, res.1}, now}
  end
) in \xs: PList. (fix (
  \f: PList -> {PList, List}. \xs: PList.
  match xs with
    pnil _ -> {xs, nil unit}
  | pcons {h, t} ->
    let res = f t in
    let hres = update res.2 h in
    {pcons {hres.1, res.1}, hres.2}
  end
) xs).1;

sum = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> + h (f t)
  end
);


/*parallel template*/

concat = fix (
  \f: List -> List -> List. \xs: List. \ys: List.
  match xs with
    nil _ -> ys
  | cons {h, t} -> cons {h, f t ys}
  end
);

parallel = \op: List -> Int.
  let pre_process = fix (
    \f: List -> Compress List. \xs: List.
    match xs with
      nil _ -> align (label (nil unit))
    | cons {h, t} ->
      let res = f t in align (label (cons {h, unlabel res}))
    end
  ) in
  let after_process = \pre: Compress List. fix (
    \f: List -> {List, Compress List}. \xs: List.
    match xs with
      nil _ -> {nil unit, pre}
    | cons {h, t} ->
      let res = f t in
      let new_tail = align (label (cons {h, unlabel res.2})) in
      let v = align (op (unlabel new_tail)) in
      {cons {v, res.1}, new_tail}
    end
  ) in \xs: PList.
  (fix (
    \f: PList -> {PList, Compress List}. \xs: PList.
    match xs with
      pnil _ -> {pnil unit, align (label (nil unit))}
    | pcons {h, t} ->
      let res = f t in
      let hinfo = pre_process h in
      let full_info = align (label (concat (unlabel hinfo) (unlabel res.2))) in
      {pcons {(after_process res.2 h).1, res.1}, full_info}
    end
  ) xs).1;

main = \xs: PList. parallel sum xs;
/*xs = pcons {cons {3, nil unit}, pcons {cons {-2, cons {1, nil unit}}, pnil unit}};
ref sum xs;
parallel sum xs;
1 1;*/