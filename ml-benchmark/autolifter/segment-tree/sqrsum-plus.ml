type list_ = Nil | Cons of int * list_
type tag = int
type result = list_
type nodeinfo = tag * result
type segtree = Empty | Node of nodeinfo * segtree * segtree
type operation = Update of int * int * tag | Query of int * int
type oplist = Onil | Ocons of operation * oplist

let rec concat x y =
  match x with
  | Cons (h, t) -> Cons (h, concat t y)
  | Nil -> y

let rec length x =
  match x with
  | Cons (_, t) -> 1 + length t
  | Nil -> 0

let map g =
  let rec f xs =
    match xs with
    | Nil -> xs
    | Cons (h, t) -> Cons (g h, f t)
  in
  f

let solve merge_tag default_tag apply target =
  let get_info n =
    match n with
    | Node (info, _, _) -> info
  in
  let merge_res x y = concat x y in
  let merge l r =
    let linfo = get_info l in
    let rinfo = get_info r in
    match linfo with
    | (_, lres) ->
      match rinfo with
      | (_, rres) -> Node ((default_tag, merge_res lres rres), l, r)
  in
  let apply n tag =
    match n with
    | Node (info, l, r) ->
      match info with
      | (itag, ires) -> Node ((merge_tag itag tag, map (apply tag) ires), l, r)
    | Empty -> Empty
  in
  let pushdown n =
    match n with
    | Node (info, l, r) ->
      match info with
      | (itag, ires) -> Node ((default_tag, ires), apply l itag, apply r itag)
    | Empty -> Empty
  in
  let buildtree =
    let rec f l r xs =
      if l = r then
        match xs with
        | Cons (h, t) -> (t, Node ((default_tag, Cons (h, Nil)), Empty, Empty))
      else
        let mid = (l + r) / 2 in
        match f l mid xs with
        | (lrest, ltree) ->
          match f (mid + 1) r lrest with
          | (rrest, rtree) -> (rrest, merge ltree rtree)
    in
    f
  in
  let update_tree ql qr tag =
    let rec f l r n =
      if l > ql || r < qr then n
      else if ql <= l && r <= qr then apply n tag
      else
        let mid = (l + r) / 2 in
        match pushdown n with
        | Node (_, lnode, rnode) ->
          let lres = f l mid lnode in
          let rres = f (mid + 1) r rnode in
          merge lres rres
        | Empty -> n
    in
    f
  in
  let query_tree ql qr =
    let rec f l r n =
      if l > ql || r < qr then (n, Nil)
      else if ql <= l && r <= qr then
        let info = get_info n in
        match info with
        | (_, ires) -> (n, ires)
      else
        let mid = (l + r) / 2 in
        let new_node = pushdown n in
        match new_node with
        | Node (info, lnode, rnode) ->
          match f l mid lnode with
          | (ln, lr) ->
            match f (mid + 1) r rnode with
            | (rn, rr) -> (Node (info, ln, rn), merge_res lr rr)
        | Empty -> (n, Nil)
    in
    f
  in
  fun init ops ->
    let len = length init in
    if len = 0 then Nil else
    let is_range_valid l r = 1 <= l && l <= r && r <= len in
    let root =
      match buildtree 1 len init with
      | (_, rt) -> rt
    in
    let rec f root ops =
      match ops with
      | Onil -> Nil
      | Ocons (h, t) ->
        match h with
        | Query (l, r) ->
          if is_range_valid l r then
            match (query_tree l r) 1 len root with
            | (new_root, res) -> Cons (target res, f new_root t)
          else f root t
        | Update (l, r, tag) ->
          if is_range_valid l r then
            let new_root = (update_tree l r tag) 1 len root in
            f new_root t
          else f root t
    in
    f root ops

let default_tag = 0

let apply_tag t w = t + w

let merge_tag x y = x + y

let rec sqrsum xs =
  match xs with
  | Nil -> 0
  | Cons (h, t) -> h * h + sqrsum t

let program = solve merge_tag default_tag apply_tag sqrsum