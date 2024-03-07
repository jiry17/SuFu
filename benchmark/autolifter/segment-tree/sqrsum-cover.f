Config ExtraGrammar = "AutoLifter";
Config NonLinear = true;

Inductive List = nil Unit | cons {Int, List};
Tag = Int; Result = Compress List; NodeInfo = {Tag, Result};
Inductive SegTree = empty Unit | node {NodeInfo, SegTree, SegTree};
Inductive Operation = update {Int, Int, Tag} | query {Int, Int};
Inductive OpList = onil Unit | ocons {Operation, OpList};

concat = fix (\f: List->List->List. \x: List. \y: List.
  match x with
    cons {h, t} -> cons {h, f t y}
  | nil _ -> y
  end
);

length = fix (\f: List->Int. \x: List.
  match x with
    cons {h, t} -> + (f t) 1
  | nil _ -> 0
  end
);

map = \g: Int -> Int. fix (
  \f: List -> List. \xs: List.
  match xs with
    nil _ -> xs
  | cons {h, t} -> cons {g h, f t}
  end
);

solve = \merge_tag: Tag -> Tag -> Tag. \default_tag: Tag.
  \apply: Tag -> Int -> Int. \target: List -> Int.
let get_info = \n: SegTree. match n with node {info, _, _} -> info end in
let merge_res = \x: Result. \y: Result. concat x y in
let merge = \l: SegTree. \r: SegTree.
  let linfo = get_info l in let rinfo = get_info r in
    node {{default_tag, merge_res linfo.2 rinfo.2}, l, r} in
let apply = \n: SegTree. \tag: Tag.
  match n with
    node {info, l, r} ->
      node {{merge_tag info.1 tag, map (apply tag) info.2}, l, r}
  end in
let pushdown = \n: SegTree.
  match n with
    node {info, l, r} ->
      node {{default_tag, info.2}, apply l info.1, apply r info.1}
  end in
let buildtree = fix (
  \f: Int -> Int -> List -> {List, SegTree}. \l: Int. \r: Int. \xs: List.
  if == l r then
    match xs with
      cons {h, t} ->
        {t, node {{default_tag, cons {h, nil unit}}, empty unit, empty unit}}
    end
  else let mid = / (+ l r) 2 in
    let lres = f l mid xs in
      let rres = f (+ mid 1) r lres.1 in
        {rres.1, merge lres.2 rres.2}
) in
let update_tree = \ql: Int. \qr: Int. \tag: Tag. fix (
  \f: Int -> Int -> SegTree -> SegTree.
  \l: Int. \r: Int. \n: SegTree.
  if or (> l ql) (< r qr) then n
  else if and (<= ql l) (<= r qr) then apply n tag
  else let mid = / (+ l r) 2 in
    match (pushdown n) with
      node {info, lnode, rnode} ->
        let lres = f l mid lnode in
        let rres = f (+ mid 1) r rnode in
          merge lres rres
    end
) in
let query_tree = \ql: Int. \qr: Int. fix (
  \f: Int -> Int -> SegTree -> {SegTree, Result}.
  \l: Int. \r: Int. \n: SegTree.
  if or (> l ql) (< r qr) then {n, nil unit}
  else if and (<= ql l) (<= r qr) then {n, (get_info n).2}
  else let mid = / (+ l r) 2 in
    let new_node = pushdown n in
    match new_node with
      node {info, lnode, rnode} ->
        let lres = f l mid lnode in
        let rres = f (+ mid 1) r rnode in
          {node {info, lres.1, rres.1}, merge_res lres.2 rres.2}
    end
) in
\init: List. \ops: OpList.
let len = length init in
if == len 0 then nil unit else
let is_range_valid = \l: Int. \r: Int. and (<= 1 l) (and (<= l r) (<= r len)) in
let root = (buildtree 1 len init).2 in
fix (
  \f: SegTree -> OpList -> List. \root: SegTree. \ops: OpList.
  match ops with
    onil _ -> nil unit
  | ocons {h, t} ->
    match h with
      query {l, r} ->
        if is_range_valid l r then
          let res = query_tree l r 1 len root in
            cons {target res.2, f res.1 t}
        else f root t
    | update {l, r, tag} ->
        if is_range_valid l r then
          let res = update_tree l r tag 1 len root in
            f res t
        else f root t
    end
  end
) root ops;


/*Tag-related operators*/

default_tag = 100;
apply_tag = \t: Tag. \w: Int.
  if == default_tag t then w else t;
merge_tag = \x: Tag. \y: Tag.
  if == default_tag y then x else y;

/*Query-related operators*/
sqrsum = fix (
  \f: List -> Int. \xs: List.
  match xs with
    nil _ -> 0
  | cons {h, t} -> + (* h h) (f t)
  end
);

main = solve merge_tag default_tag apply_tag sqrsum;
