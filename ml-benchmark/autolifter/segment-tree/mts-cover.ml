type tlist = Nil of unit | Cons of int * tlist
type tag = int
type result = tlist
type nodeinfo = tag * result
type segtree = Empty of unit | Node of nodeinfo * segtree * segtree
type operation = Update of int * int * tag | Query of int * int
type oplist = Onil of unit | Ocons of operation * oplist

let rec concat x y =
  match x with
  | Cons (h, t) -> Cons (h, concat t y)
  | Nil _ -> y

let rec length x =
  match x with
  | Cons (_, t) -> 1 + length t
  | Nil _ -> 0

let map g =
  let rec f xs =
    match xs with
    | Nil _ -> xs
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
    let lres =
      match linfo with
      | (_, res) -> res
    in
    let rres =
      match rinfo with
      | (_, res) -> res
    in
    Node ((default_tag, merge_res lres rres), l, r)
  in
  let apply_node n tagv =
    match n with
    | Node (info, l, r) ->
      let new_tag =
        match info with
        | (tg, _) -> merge_tag tg tagv
      in
      let res =
        match info with
        | (_, rs) -> rs
      in
      Node ((new_tag, (map (apply tagv)) res), l, r)
  in
  let pushdown n =
    match n with
    | Node (info, l, r) ->
      let tg =
        match info with
        | (t, _) -> t
      in
      let rs =
        match info with
        | (_, rs) -> rs
      in
      Node ((default_tag, rs), apply_node l tg, apply_node r tg)
  in
  let rec buildtree l r xs =
    if l = r then
      match xs with
      | Cons (h, t) -> (t, Node ((default_tag, Cons (h, Nil ())), Empty (), Empty ()))
    else
      let mid = (l + r) / 2 in
      let lres = buildtree l mid xs in
      let xs2 =
        match lres with
        | (rest, _) -> rest
      in
      let rres = buildtree (mid + 1) r xs2 in
      let rrest =
        match rres with
        | (rest, _) -> rest
      in
      let lt =
        match lres with
        | (_, t) -> t
      in
      let rt =
        match rres with
        | (_, t) -> t
      in
      (rrest, merge lt rt)
  in
  let update_tree ql qr tagv =
    let rec f l r n =
      if (l > ql) || (r < qr) then n
      else if (ql <= l) && (r <= qr) then apply_node n tagv
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
      if (l > ql) || (r < qr) then (n, Nil ())
      else if (ql <= l) && (r <= qr) then
        let inf = get_info n in
        let rs =
          match inf with
          | (_, rs) -> rs
        in
        (n, rs)
      else
        let mid = (l + r) / 2 in
        let new_node = pushdown n in
        match new_node with
        | Node (info, lnode, rnode) ->
          let lres = f l mid lnode in
          let rres = f (mid + 1) r rnode in
          let ln =
            match lres with
            | (nn, _) -> nn
          in
          let rn =
            match rres with
            | (nn, _) -> nn
          in
          let lr =
            match lres with
            | (_, rr) -> rr
          in
          let rr =
            match rres with
            | (_, rr) -> rr
          in
          (Node (info, ln, rn), merge_res lr rr)
    in
    f
  in
  fun init ops ->
    let len = length init in
    if len = 0 then Nil ()
    else
      let is_range_valid l r = (1 <= l) && (l <= r) && (r <= len) in
      let built = buildtree 1 len init in
      let root =
        match built with
        | (_, t) -> t
      in
      let rec loop root ops =
        match ops with
        | Onil _ -> Nil ()
        | Ocons (h, t) ->
          (match h with
           | Query (l, r) ->
             if is_range_valid l r then
               let res = (query_tree l r) 1 len root in
               let new_root =
                 match res with
                 | (nr, _) -> nr
               in
               let qres =
                 match res with
                 | (_, rr) -> rr
               in
               Cons (target qres, loop new_root t)
             else
               loop root t
           | Update (l, r, tagv) ->
             if is_range_valid l r then
               let res = (update_tree l r tagv) 1 len root in
               loop res t
             else
               loop root t)
      in
      loop root ops

let default_tag = 100
let apply_tag t w = if default_tag = t then w else t
let merge_tag x y = if default_tag = y then x else y
let max a b = if a < b then b else a

let mts xs =
  let rec f xs =
    match xs with
    | Nil _ -> (0, 0)
    | Cons (h, t) ->
      let res = f t in
      let a =
        match res with
        | (a, _) -> a
      in
      let b =
        match res with
        | (_, b) -> b
      in
      (max (b + h) a, b + h)
  in
  let r = f xs in
  match r with
  | (a, _) -> a

let program = solve merge_tag default_tag apply_tag mts