type list_ = Nil | Cons of int * list_
type tag = int
type result_ = list_
type nodeinfo = { t: tag; res: result_ }
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

let solve merge_tag default_tag apply_value target =
  let get_info n =
    match n with
    | Node (info, _, _) -> info
  in
  let merge_res x y = concat x y in
  let merge l r =
    let linfo = get_info l in
    let rinfo = get_info r in
    Node ({ t = default_tag; res = merge_res linfo.res rinfo.res }, l, r)
  in
  let apply_to_node n tag =
    match n with
    | Node (info, l, r) ->
        let newt = merge_tag info.t tag in
        let newres = map (fun w -> apply_value tag w) info.res in
        Node ({ t = newt; res = newres }, l, r)
    | Empty -> Empty
  in
  let pushdown n =
    match n with
    | Node (info, l, r) ->
        let l2 = apply_to_node l info.t in
        let r2 = apply_to_node r info.t in
        Node ({ t = default_tag; res = info.res }, l2, r2)
    | Empty -> Empty
  in
  let rec buildtree l r xs =
    if l = r then
      match xs with
      | Cons (h, t) -> (t, Node ({ t = default_tag; res = Cons (h, Nil) }, Empty, Empty))
    else
      let mid = (l + r) / 2 in
      let lres = buildtree l mid xs in
      match lres with
      | (xs1, lt) ->
          let rres = buildtree (mid + 1) r xs1 in
          match rres with
          | (xs2, rt) -> (xs2, merge lt rt)
  in
  let update_tree ql qr tag =
    let rec f l r n =
      if (l > ql) || (r < qr) then n
      else if (ql <= l) && (r <= qr) then apply_to_node n tag
      else
        let mid = (l + r) / 2 in
        let new_node = pushdown n in
        match new_node with
        | Node (info, lnode, rnode) ->
            let lres = f l mid lnode in
            let rres = f (mid + 1) r rnode in
            merge lres rres
        | Empty -> new_node
    in
    f
  in
  let query_tree ql qr =
    let rec f l r n =
      if (l > ql) || (r < qr) then (n, Nil)
      else if (ql <= l) && (r <= qr) then
        let info = get_info n in
        (n, info.res)
      else
        let mid = (l + r) / 2 in
        let new_node = pushdown n in
        match new_node with
        | Node (info, lnode, rnode) ->
            let lres = f l mid lnode in
            match lres with
            | (ln, lv) ->
                let rres = f (mid + 1) r rnode in
                match rres with
                | (rn, rv) -> (Node (info, ln, rn), merge_res lv rv)
        | Empty -> (new_node, Nil)
    in
    f
  in
  fun init ops ->
    let len = length init in
    if len = 0 then Nil
    else
      let is_range_valid l r = (1 <= l) && (l <= r) && (r <= len) in
      let tmp_root = buildtree 1 len init in
      let root =
        match tmp_root with
        | (_, rt) -> rt
      in
      let rec process root ops =
        match ops with
        | ONil -> Nil
        | OCons (h, t) ->
            match h with
            | Query (l, r) ->
                if is_range_valid l r then
                  let res = query_tree l r 1 len root in
                  match res with
                  | (nr, v) -> Cons (target v, process nr t)
                else process root t
            | Update (l, r, tag) ->
                if is_range_valid l r then
                  let res = update_tree l r tag 1 len root in
                  process res t
                else process root t
      in
      process root ops

let default_tag = 100
let apply_tag t w = if default_tag = t then w else t
let merge_tag x y = if default_tag = y then x else y

let rec sqrsum xs =
  match xs with
  | Nil -> 0
  | Cons (h, t) -> (h * h) + sqrsum t

let program = solve merge_tag default_tag apply_tag sqrsum