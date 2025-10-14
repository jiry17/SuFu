type listx = Nil | Cons of int * listx

type segtree = Empty | Node of (bool * listx) * segtree * segtree

type operation = Update of int * int * bool | Query of int * int

type oplist = Onil | Ocons of operation * oplist

let rec concat x y =
  match x with
  | Cons (h, t) -> Cons (h, concat t y)
  | Nil -> y

let rec length x =
  match x with
  | Cons (_, t) -> length t + 1
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
      | (_, rres) ->
        Node ((default_tag, merge_res lres rres), l, r)
  in
  let apply_node n tag =
    match n with
    | Node (info, l, r) ->
      match info with
      | (itag, ires) ->
        Node ((merge_tag itag tag, map (apply tag) ires), l, r)
    | Empty -> Empty
  in
  let pushdown n =
    match n with
    | Node (info, l, r) ->
      match info with
      | (itag, ires) ->
        Node ((default_tag, ires), apply_node l itag, apply_node r itag)
    | Empty -> Empty
  in
  let rec buildtree l r xs =
    if l = r then
      match xs with
      | Cons (h, t) -> (t, Node ((default_tag, Cons (h, Nil)), Empty, Empty))
    else
      let mid = (l + r) / 2 in
      let lres = buildtree l mid xs in
      match lres with
      | (lxs, ltree) ->
        let rres = buildtree (mid + 1) r lxs in
        match rres with
        | (rxs, rtree) -> (rxs, merge ltree rtree)
  in
  let update_tree ql qr tag =
    let rec f l r n =
      if l > ql || r < qr then n
      else if ql <= l && r <= qr then apply_node n tag
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
          let lres = f l mid lnode in
          match lres with
          | (ln, lr) ->
            let rres = f (mid + 1) r rnode in
            match rres with
            | (rn, rr) -> (Node (info, ln, rn), merge_res lr rr)
        | Empty -> (n, Nil)
    in
    f
  in
  fun init ops ->
    let len = length init in
    if len = 0 then Nil
    else
      let is_range_valid l r = 1 <= l && l <= r && r <= len in
      let root =
        let res = buildtree 1 len init in
        match res with
        | (_, rt) -> rt
      in
      let rec f root ops =
        match ops with
        | Onil -> Nil
        | Ocons (h, t) ->
          match h with
          | Query (l, r) ->
            if is_range_valid l r then
              let res = query_tree l r 1 len root in
              match res with
              | (new_root, res_list) -> Cons (target res_list, f new_root t)
            else f root t
          | Update (l, r, tag) ->
            if is_range_valid l r then
              let res = update_tree l r tag 1 len root in
              f res t
            else f root t
      in
      f root ops

let default_tag = false

let apply_tag t w = if t then 0 - w else w

let merge_tag x y = if x then not y else y

let inf = 100

let ntwo = -2

let min a b = if a < b then a else b

let max a b = if a < b then b else a

let trdmin xs =
  let rec f xs =
    match xs with
    | Nil -> (inf, inf, inf)
    | Cons (h, t) ->
      let res = f t in
      match res with
      | (a, b, c) -> (min a (max b h), min b (max c h), min c h)
  in
  let res = f xs in
  match res with
  | (x, _, _) -> x

let program = solve merge_tag default_tag apply_tag trdmin