type list_ = Nil | Cons of int * list_
type tag = bool
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
        let info = (default_tag, merge_res lres rres) in
        Node (info, l, r)
  in
  let apply n tag =
    match n with
    | Node (info, l, r) ->
      match info with
      | (itag, ires) ->
        let new_tag = merge_tag itag tag in
        let g = apply tag in
        let new_res = map g ires in
        let new_info = (new_tag, new_res) in
        Node (new_info, l, r)
    | Empty -> Empty
  in
  let pushdown n =
    match n with
    | Node (info, l, r) ->
      match info with
      | (itag, ires) ->
        let new_l = apply l itag in
        let new_r = apply r itag in
        Node ((default_tag, ires), new_l, new_r)
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
      | (xs1, lnode) ->
        let rres = buildtree (mid + 1) r xs1 in
        match rres with
        | (xs2, rnode) -> (xs2, merge lnode rnode)
  in
  let update_tree ql qr tag =
    let rec f l r n =
      if l > qr || r < ql then n
      else if ql <= l && r <= qr then apply n tag
      else
        let mid = (l + r) / 2 in
        let n1 = pushdown n in
        match n1 with
        | Node (info, lnode, rnode) ->
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
        | (_, ires) -> (n, ires)
      else
        let mid = (l + r) / 2 in
        let new_node = pushdown n in
        match new_node with
        | Node (info, lnode, rnode) ->
          let lres = f l mid lnode in
          let rres = f (mid + 1) r rnode in
          match lres with
          | (lnode2, lresult) ->
            match rres with
            | (rnode2, rresult) ->
              (Node (info, lnode2, rnode2), merge_res lresult rresult)
        | Empty -> (n, Nil)
    in
    f
  in
  fun init ops ->
    let len = length init in
    if len = 0 then Nil else
    let is_range_valid l r = 1 <= l && l <= r && r <= len in
    let root_pair = buildtree 1 len init in
    match root_pair with
    | (_, root) ->
      let rec f root ops =
        match ops with
        | Onil -> Nil
        | Ocons (h, t) ->
          match h with
          | Query (l, r) ->
            if is_range_valid l r then
              let res = query_tree l r 1 len root in
              match res with
              | (root2, result) ->
                let v = target result in
                Cons (v, f root2 t)
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

let max a b = if a < b then b else a

let mts xs =
  let rec f xs =
    match xs with
    | Nil -> (0, 0)
    | Cons (h, t) ->
      let res = f t in
      match res with
      | (m, s) ->
        let m2 = max (s + h) m in
        let s2 = s + h in
        (m2, s2)
  in
  let r = f xs in
  match r with
  | (m, _) -> m

let program = solve merge_tag default_tag apply_tag mts