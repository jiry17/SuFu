type lst = Nil | Cons of int * lst
type tag = int
type result = lst
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
  | Cons (_, t) -> length t + 1
  | Nil -> 0

let rec map g xs =
  match xs with
  | Nil -> xs
  | Cons (h, t) -> Cons (g h, map g t)

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
  let apply_tree n tag =
    match n with
    | Node (info, l, r) ->
      match info with
      | (itag, ires) ->
        Node ((merge_tag itag tag, map (apply tag) ires), l, r)
  in
  let pushdown n =
    match n with
    | Node (info, l, r) ->
      match info with
      | (itag, ires) ->
        Node ((default_tag, ires), apply_tree l itag, apply_tree r itag)
  in
  let rec buildtree l r xs =
    if l = r then
      match xs with
      | Cons (h, t) ->
        (t, Node ((default_tag, Cons (h, Nil)), Empty, Empty))
    else
      let mid = (l + r) / 2 in
      let lres = buildtree l mid xs in
      match lres with
      | (xs1, st1) ->
        let rres = buildtree (mid + 1) r xs1 in
        match rres with
        | (xs2, st2) ->
          (xs2, merge st1 st2)
  in
  let update_tree ql qr tag =
    let rec f l r n =
      if (l > ql) || (r < qr) then n
      else if (ql <= l) && (r <= qr) then apply_tree n tag
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
          | (lnode2, lresult) ->
            let rres = f (mid + 1) r rnode in
            match rres with
            | (rnode2, rresult) ->
              (Node (info, lnode2, rnode2), merge_res lresult rresult)
    in
    f
  in
  fun init ops ->
    let len = length init in
    if len = 0 then
      Nil
    else
      let is_range_valid l r = (1 <= l) && (l <= r) && (r <= len) in
      let x = buildtree 1 len init in
      match x with
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
                | (new_root, result) ->
                  Cons (target result, f new_root t)
              else
                f root t
            | Update (l, r, tag) ->
              if is_range_valid l r then
                let res = update_tree l r tag 1 len root in
                f res t
              else
                f root t
        in
        f root ops

let default_tag = 0
let apply_tag t w = t + w
let merge_tag x y = x + y

let rec sum xs =
  match xs with
  | Nil -> 0
  | Cons (h, t) -> h + sum t

let program = solve merge_tag default_tag apply_tag sum