type list_ = Nil | Cons of int * list_
type tag = bool
type result_ = list_
type nodeinfo = tag * result_
type segtree = Empty | Node of nodeinfo * segtree * segtree
type operation = Update of int * int * tag | Query of int * int
type oplist = ONil | OCons of operation * oplist

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
      | (it, ir) ->
        Node ((merge_tag it tag, map (apply tag) ir), l, r)
    | Empty -> Empty
  in
  let pushdown n =
    match n with
    | Node (info, l, r) ->
      match info with
      | (it, ir) ->
        Node ((default_tag, ir), apply_node l it, apply_node r it)
    | Empty -> Empty
  in
  let buildtree =
    let rec f l r xs =
      if l = r then
        match xs with
        | Cons (h, t) ->
          (t, Node ((default_tag, Cons (h, Nil)), Empty, Empty))
        | Nil -> (Nil, Empty)
      else
        let mid = (l + r) / 2 in
        let lres = f l mid xs in
        match lres with
        | (lxs, ltree) ->
          let rres = f (mid + 1) r lxs in
          match rres with
          | (rxs, rtree) ->
            (rxs, merge ltree rtree)
    in
    f
  in
  let update_tree ql qr tag =
    let rec f l r n =
      if l > qr || r < ql then n
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
      if l > qr || r < ql then (n, Nil)
      else if ql <= l && r <= qr then
        let info = get_info n in
        match info with
        | (_, res) -> (n, res)
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
        | Empty -> (n, Nil)
    in
    f
  in
  fun init ops ->
    let len = length init in
    if len = 0 then Nil else
    let is_range_valid l r = 1 <= l && l <= r && r <= len in
    let build_res = buildtree 1 len init in
    match build_res with
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
              | (new_root, res_list) ->
                Cons (target res_list, f new_root t)
            else f root t
          | Update (l, r, tag_) ->
            if is_range_valid l r then
              let new_root = update_tree l r tag_ 1 len root in
              f new_root t
            else f root t
      in
      f root ops

let default_tag = false
let apply_tag t w = if t then 0 - w else w
let merge_tag x y = if x then not y else y
let inf = 100
let rec min xs =
  match xs with
  | Nil -> inf
  | Cons (h, t) ->
    let res = min t in
    if res < h then res else h

let program = solve merge_tag default_tag apply_tag min