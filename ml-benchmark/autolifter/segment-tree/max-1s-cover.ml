type list_ = Nil | Cons of int * list_
type tag = int
type result = list_
type nodeinfo = tag * result
type segtree = Empty | Node of nodeinfo * segtree * segtree
type operation = Update of int * int * tag | Query of int * int
type oplist = ONil | OCons of operation * oplist

let rec concat x y =
  match x with
  | Cons (h, t) -> Cons (h, concat t y)
  | Nil -> y

let rec length x =
  match x with
  | Cons (_, t) -> (length t) + 1
  | Nil -> 0

let map g =
  let rec f xs =
    match xs with
    | Nil -> xs
    | Cons (h, t) -> Cons (g h, f t)
  in
  f

let solve merge_tag default_tag apply_tag target =
  let get_info n =
    match n with
    | Node (info, _, _) -> info
  in
  let merge_res x y = concat x y in
  let merge l r =
    let linfo = get_info l in
    let rinfo = get_info r in
    match linfo with
    | (ltag, lres) ->
      match rinfo with
      | (rtag, rres) -> Node ((default_tag, merge_res lres rres), l, r)
  in
  let apply_node n tag =
    match n with
    | Node (info, l, r) ->
      match info with
      | (itag, ires) ->
        Node ((merge_tag itag tag, map (apply_tag tag) ires), l, r)
  in
  let pushdown n =
    match n with
    | Node (info, l, r) ->
      match info with
      | (itag, ires) ->
        Node ((default_tag, ires), apply_node l itag, apply_node r itag)
  in
  let rec buildtree_f l r xs =
    if l = r then
      match xs with
      | Cons (h, t) -> (t, Node ((default_tag, Cons (h, Nil)), Empty, Empty))
    else
      let mid = (l + r) / 2 in
      let lres = buildtree_f l mid xs in
      match lres with
      | (xs2, left) ->
        let rres = buildtree_f (mid + 1) r xs2 in
        match rres with
        | (xs3, right) -> (xs3, merge left right)
  in
  let buildtree = buildtree_f in
  let update_tree ql qr tag =
    let rec f l r n =
      if (l > ql) || (r < qr) then n
      else if (ql <= l) && (r <= qr) then apply_node n tag
      else
        let mid = (l + r) / 2 in
        let new_node = pushdown n in
        match new_node with
        | Node (_, lnode, rnode) ->
          let lres = f l mid lnode in
          let rres = f (mid + 1) r rnode in
          merge lres rres
    in
    f
  in
  let query_tree ql qr =
    let rec f l r n =
      if (l > ql) || (r < qr) then (n, Nil)
      else if (ql <= l) && (r <= qr) then
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
          | (lnode2, lres2) ->
            let rres = f (mid + 1) r rnode in
            match rres with
            | (rnode2, rres2) ->
              (Node (info, lnode2, rnode2), merge_res lres2 rres2)
    in
    f
  in
  fun init ops ->
    let len = length init in
    if len = 0 then Nil else
    let is_range_valid l r = (1 <= l) && (l <= r) && (r <= len) in
    let built = buildtree 1 len init in
    match built with
    | (_, root) ->
      let rec f root ops =
        match ops with
        | ONil -> Nil
        | OCons (h, t) ->
          match h with
          | Query (l, r) ->
            if is_range_valid l r then
              let res = query_tree l r 1 len root in
              match res with
              | (new_root, result) -> Cons (target result, f new_root t)
            else f root t
          | Update (l, r, tag) ->
            if is_range_valid l r then
              let res = update_tree l r tag 1 len root in
              f res t
            else f root t
      in
      f root ops

let default_tag = 100
let apply_tag t w =
  if default_tag = t then w else t
let merge_tag x y =
  if default_tag = y then x else y

let fit_int a =
  if (2 * (a / 2)) = a then 0 else 1

let rec fit_list xs =
  match xs with
  | Nil -> xs
  | Cons (h, t) -> Cons (fit_int h, fit_list t)

let rec fit_ops xs =
  match xs with
  | ONil -> xs
  | OCons (Update (l, r, tag), t) -> OCons (Update (l, r, fit_int tag), fit_ops t)
  | OCons (h, t) -> OCons (h, fit_ops t)

let max a b = if a < b then b else a

let max1s xs =
  let rec f ma l xs =
    match xs with
    | Nil -> max ma l
    | Cons (h, t) ->
      if h = 0 then f (max ma l) 0 t else f ma (l + 1) t
  in
  f 0 0 xs

let program raw_init raw_ops =
  let init = fit_list raw_init in
  let ops = fit_ops raw_ops in
  solve merge_tag default_tag apply_tag max1s init ops