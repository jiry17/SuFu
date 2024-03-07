Config ComposeNum = 4;

Inductive List = elt Int | cons {Int, List};
Inductive NList = line List | ncons {List, NList};

max = \a: Int. \b: Int. if < a b then b else a;
min = \a: Int. \b: Int. if > a b then b else a;

interval = fix (
  \f: List -> {Int, Int}. \xs: List.
  match xs with
    elt a -> {a,a}
  | cons {hd, tl} ->
    let result = f tl in
    {min hd result.1, max hd result.2}
  end
);

spec = \xs: NList. (fix (
  \f: NList -> {Int, Int, Bool}. \xs: NList.
  match xs with
    line a -> 
    let result = interval a in
    {result.1, result.2, true}
  | ncons {hd, tl} ->
    let r1 = f tl in
    let r2 = interval hd in
    {min r1.1 r2.1, max r1.2 r2.2, and r1.3 (and (<= r1.1 r2.1) (<= r1.2 r2.2))}
  end
) xs).3;

target = fix (
  \f: NList -> Compress NList. \xs: NList.
  match xs with
    line x ->
      let info = interval x in /*This invocation is included in Synduce's template*/
        xs
  | ncons {h, t} ->
      let info = interval h in /*This invocation is included in Synduce's template*/
        ncons {h, f t}
  end
);

main = \xs: NList. spec (target xs);