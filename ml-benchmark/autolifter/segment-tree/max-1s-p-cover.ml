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

let map g =
  let rec f xs =
    match xs with
    | Nil -> xs
    | Cons (h, t) -> Cons (g h, f t)
  in
  f

let solve merge_tag default_tag apply_elem target =
  let get_info n =
    match n with
    | Node (info, _, _) -> info
    | _ -> failwith ""
  in
  let merge_res x y = concat x y in
  let merge l r =
    let linfo = get_info l in
    let rinfo = get_info r in
    match linfo with
    | (ltag, lres) ->
      match rinfo with
      | (_, rres) ->
        Node ((default_tag, merge_res lres rres), l, r)
  in
  let apply_node n tagv =
    match n with
    | Node (info, l, r) ->
      (match info with
       | (itag, ires) ->
         Node ((merge_tag itag tagv, map (apply_elem tagv) ires), l, r))
    | _ -> n
  in
  let pushdown n =
    match n with
    | Node (info, l, r) ->
      (match info with
       | (itag, ires) ->
         Node ((default_tag, ires), apply_node l itag, apply_node r itag))
    | _ -> n
  in
  let rec buildtree l r xs =
    if l = r then
      match xs with
      | Cons (h, t) ->
        let leaf = Node ((default_tag, Cons (h, Nil)), Empty, Empty) in
        (t, leaf)
      | _ -> (Nil, Empty)
    else
      let mid = (l + r) / 2 in
      let lres = buildtree l mid xs in
      match lres with
      | (rest1, ltree) ->
        let rres = buildtree (mid + 1) r rest1 in
        match rres with
        | (rest2, rtree) ->
          (rest2, merge ltree rtree)
  in
  let update_tree ql qr tagv =
    let rec f l r n =
      if l > qr || r < ql then n
      else if ql <= l && r <= qr then apply_node n tagv
      else
        let mid = (l + r) / 2 in
        let n2 = pushdown n in
        match n2 with
        | Node (_, lnode, rnode) ->
          let lres = f l mid lnode in
          let rres = f (mid + 1) r rnode in
          merge lres rres
        | _ -> n2
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
          (match lres with
           | (lnode2, lacc) ->
             let rres = f (mid + 1) r rnode in
             (match rres with
              | (rnode2, racc) ->
                (Node (info, lnode2, rnode2), merge_res lacc racc)))
        | _ -> (new_node, Nil)
    in
    f
  in
  fun init ops ->
    let len = length init in
    if len = 0 then Nil else
    let is_range_valid l r = 1 <= l && l <= r && r <= len in
    let root2 =
      let br = buildtree 1 len init in
      match br with
      | (_, tree) -> tree
    in
    let rec loop root ops =
      match ops with
      | ONil -> Nil
      | OCons (h, t) ->
        (match h with
         | Query (l, r) ->
           if is_range_valid l r then
             let res = query_tree l r 1 len root in
             (match res with
              | (root3, acc) ->
                let tg = target acc in
                (match tg with
                 | (a, _) ->
                   Cons (a, loop root3 t)))
           else loop root t
         | Update (l, r, tagv) ->
           if is_range_valid l r then
             let root3 = update_tree l r tagv 1 len root in
             loop root3 t
           else loop root t)
    in
    loop root2 ops

let default_tag = 100

let apply_tag t w =
  if default_tag = t then w else t

let merge_tag x y =
  if default_tag = y then x else y

let fit_int a =
  if 2 * (a / 2) = a then 0 else 1

let rec fit_list xs =
  match xs with
  | Nil -> xs
  | Cons (h, t) -> Cons (fit_int h, fit_list t)

let rec fit_ops xs =
  match xs with
  | ONil -> xs
  | OCons (op, t) ->
    (match op with
     | Update (l, r, tagv) ->
       OCons (Update (l, r, fit_int tagv), fit_ops t)
     | Query _ -> OCons (op, fit_ops t))

let rec max1s_with_pos_f pre i xs =
  match xs with
  | Nil ->
    let len = i - pre in
    (len, pre)
  | Cons (h, t) ->
    if h = 1 then max1s_with_pos_f pre (i + 1) t
    else
      let len = i - pre in
      let res = max1s_with_pos_f (i + 1) (i + 1) t in
      match res with
      | (len2, _) ->
        if len >= len2 then (len, pre) else res

let max1s_with_pos = max1s_with_pos_f 0 0

let program raw_init raw_ops =
  let init = fit_list raw_init in
  let ops = fit_ops raw_ops in
  let s = solve merge_tag default_tag apply_tag max1s_with_pos in
  s init ops