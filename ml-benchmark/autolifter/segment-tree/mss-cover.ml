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
  | Cons (_, t) -> (length t) + 1
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
    | (ltag, lres) ->
      match rinfo with
      | (rtag, rres) ->
        Node ((default_tag, merge_res lres rres), l, r)
  in
  let apply_node n tag_ =
    match n with
    | Node (info, l, r) ->
      match info with
      | (itag, ires) ->
        Node ((merge_tag itag tag_, (map (apply tag_)) ires), l, r)
  in
  let pushdown n =
    match n with
    | Node (info, l, r) ->
      match info with
      | (itag, ires) ->
        Node ((default_tag, ires), apply_node l itag, apply_node r itag)
  in
  let rec buildtree l r xs =
    if l = r then
      match xs with
      | Cons (h, t) -> (t, Node ((default_tag, Cons (h, Nil)), Empty, Empty))
    else
      let mid = (l + r) / 2 in
      let lres = buildtree l mid xs in
      match lres with
      | (xs2, ln) ->
        let rres = buildtree (mid + 1) r xs2 in
        match rres with
        | (xs3, rn) -> (xs3, merge ln rn)
  in
  let update_tree ql qr tag_ =
    let rec f l r n =
      if (l > ql) || (r < qr) then n
      else if (ql <= l) && (r <= qr) then apply_node n tag_
      else
        let mid = (l + r) / 2 in
        let newn = pushdown n in
        match newn with
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
    let rootpair = buildtree 1 len init in
    match rootpair with
    | (_, root) ->
      let rec f root ops =
        match ops with
        | Onil -> Nil
        | Ocons (h, t) ->
          match h with
          | Query (l, r) ->
            if is_range_valid l r then
              let res = (query_tree l r) 1 len root in
              match res with
              | (root2, result2) ->
                Cons (target result2, f root2 t)
            else f root t
          | Update (l, r, tag_) ->
            if is_range_valid l r then
              let res = (update_tree l r tag_) 1 len root in
              f res t
            else f root t
      in
      f root ops

let default_tag = 100
let apply_tag t w = if default_tag = t then w else t
let merge_tag x y = if default_tag = y then x else y
let max a b = if a < b then b else a

let mss xs =
  let rec f xs =
    match xs with
    | Nil -> (0, 0)
    | Cons (h, t) ->
      let res = f t in
      match res with
      | (r1, r2) ->
        let new_mps = max 0 (h + r2) in
        (max new_mps r1, new_mps)
  in
  let pair = f xs in
  match pair with
  | (v, _) -> v

let program = solve merge_tag default_tag apply_tag mss