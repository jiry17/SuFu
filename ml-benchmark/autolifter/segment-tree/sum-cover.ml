type list_ = Nil of unit | Cons of int * list_
type nodeinfo = int * list_
type segtree = Empty of unit | Node of nodeinfo * segtree * segtree
type operation = Update of int * int * int | Query of int * int
type oplist = Onil of unit | Ocons of operation * oplist

let rec concat x y =
  match x with
  | Cons (h, t) -> Cons (h, concat t y)
  | Nil _ -> y

let rec length x =
  match x with
  | Cons (_, t) -> 1 + length t
  | Nil _ -> 0

let map_ g =
  let rec f xs =
    match xs with
    | Nil _ -> xs
    | Cons (h, t) -> Cons (g h, f t)
  in
  f

let solve merge_tag default_tag apply_fn target =
  let get_info n =
    match n with
    | Node (info, _, _) -> info
  in
  let merge_res x y = concat x y in
  let merge l r =
    let linfo = get_info l in
    match linfo with
    | (_, lres) ->
      let rinfo = get_info r in
      match rinfo with
      | (_, rres) ->
        Node ((default_tag, merge_res lres rres), l, r)
  in
  let apply_node n tag =
    match n with
    | Node (info, l, r) ->
      match info with
      | (itag, ires) ->
        Node ((merge_tag itag tag, map_ (fun w -> apply_fn tag w) ires), l, r)
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
      | Cons (h, t) ->
        (t, Node ((default_tag, Cons (h, Nil ())), Empty (), Empty ()))
    else
      let mid = (l + r) / 2 in
      let lres = buildtree l mid xs in
      match lres with
      | (rest1, left) ->
        let rres = buildtree (mid + 1) r rest1 in
        match rres with
        | (rest2, right) ->
          (rest2, merge left right)
  in
  let update_tree ql qr tag =
    let rec f l r n =
      if (l > ql) || (r < qr) then n
      else if (ql <= l) && (r <= qr) then apply_node n tag
      else
        let mid = (l + r) / 2 in
        let pushed = pushdown n in
        match pushed with
        | Node (_, lnode, rnode) ->
          let lres = f l mid lnode in
          let rres = f (mid + 1) r rnode in
          merge lres rres
    in
    f
  in
  let query_tree ql qr =
    let rec f l r n =
      if (l > ql) || (r < qr) then (n, Nil ())
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
          | (n1, res1) ->
            let rres = f (mid + 1) r rnode in
            match rres with
            | (n2, res2) ->
              (Node (info, n1, n2), merge_res res1 res2)
    in
    f
  in
  fun init ops ->
    let len = length init in
    if len = 0 then Nil ()
    else
      let is_range_valid l r = (1 <= l) && (l <= r) && (r <= len) in
      let bt = buildtree 1 len init in
      let root =
        match bt with
        | (_, tree) -> tree
      in
      let rec f root ops =
        match ops with
        | Onil _ -> Nil ()
        | Ocons (h, t) ->
          match h with
          | Query (l, r) ->
            if is_range_valid l r then
              let qf = query_tree l r in
              let res = qf 1 len root in
              match res with
              | (new_root, reslst) ->
                Cons (target reslst, f new_root t)
            else f root t
          | Update (l, r, tag) ->
            if is_range_valid l r then
              let uf = update_tree l r tag in
              let res = uf 1 len root in
              f res t
            else f root t
      in
      f root ops

let default_tag = 100
let apply_tag t w = if default_tag = t then w else t
let merge_tag x y = if default_tag = y then x else y

let rec sum xs =
  match xs with
  | Nil _ -> 0
  | Cons (h, t) -> h + sum t

let program = solve merge_tag default_tag apply_tag sum