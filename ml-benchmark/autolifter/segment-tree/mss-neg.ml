type list_ = Nil | Cons of int * list_
type segtree = Empty | Node of (bool * list_) * segtree * segtree
type operation = Update of int * int * bool | Query of int * int
type op_list = Onil | Ocons of operation * op_list

let rec concat x y =
  match x with
  | Cons (h, t) -> Cons (h, concat t y)
  | Nil -> y

let rec length x =
  match x with
  | Cons (_, t) -> length t + 1
  | Nil -> 0

let rec map g xs =
  match xs with
  | Nil -> Nil
  | Cons (h, t) -> Cons (g h, map g t)

let solve merge_tag default_tag apply target =
  let get_info n =
    match n with
    | Node (info, _, _) -> info
    | Empty -> (default_tag, Nil)
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
  let apply_node n tag =
    match n with
    | Node (info, l, r) ->
      match info with
      | (tg, res) -> Node ((merge_tag tg tag, map (apply tag) res), l, r)
    | Empty -> n
  in
  let pushdown n =
    match n with
    | Node (info, l, r) ->
      match info with
      | (tg, res) -> Node ((default_tag, res), apply_node l tg, apply_node r tg)
    | Empty -> n
  in
  let rec buildtree l r xs =
    if l = r then
      match xs with
      | Cons (h, t) -> (t, Node ((default_tag, Cons (h, Nil)), Empty, Empty))
    else
      let mid = (l + r) / 2 in
      let lres = buildtree l mid xs in
      match lres with
      | (xs1, ltree) ->
        let rres = buildtree (mid + 1) r xs1 in
        match rres with
        | (xs2, rtree) -> (xs2, merge ltree rtree)
  in
  let update_tree ql qr tag =
    let rec f l r n =
      if (l > ql) || (r < qr) then n
      else if (ql <= l) && (r <= qr) then apply_node n tag
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
      if (l > ql) || (r < qr) then (n, Nil)
      else if (ql <= l) && (r <= qr) then
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
            | (rnode2, rres2) -> (Node (info, lnode2, rnode2), merge_res lres2 rres2)
        | Empty -> (n, Nil)
    in
    f
  in
  fun init ops ->
    let len = length init in
    if len = 0 then Nil else
    let is_range_valid l r = (1 <= l) && (l <= r) && (r <= len) in
    let root_pair = buildtree 1 len init in
    match root_pair with
    | (_, root) ->
      let rec loop root ops =
        match ops with
        | Onil -> Nil
        | Ocons (h, t) ->
          match h with
          | Query (l, r) ->
            if is_range_valid l r then
              let res = query_tree l r 1 len root in
              match res with
              | (new_root, res_list) -> Cons (target res_list, loop new_root t)
            else loop root t
          | Update (l, r, tag) ->
            if is_range_valid l r then
              let resf = update_tree l r tag in
              let res = resf 1 len root in
              loop res t
            else loop root t
      in
      loop root ops

let default_tag = false
let apply_tag t w = if t then 0 - w else w
let merge_tag x y = if x then not y else y
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
  let p = f xs in
  match p with
  | (v, _) -> v

let program = solve merge_tag default_tag apply_tag mss