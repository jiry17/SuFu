type list_ = Cons of int * list_ | Nil
type cartTree = Node of cartTree * int * cartTree | Leaf
type cartPath = ConsNode of cartTree * cartPath | NilNode

let rec concat x y =
  match x with
  | Cons (h, t) -> Cons (h, concat t y)
  | Nil -> y

let rec cart2l t =
  match t with
  | Node (l, w, r) ->
      (match l with
       | Leaf ->
           (match r with
            | Leaf -> Cons (w, Nil)
            | _ ->
                let rres = cart2l r in
                concat (Cons (w, Nil)) rres))
       | _ ->
           (match r with
            | Leaf ->
                let lres = cart2l l in
                concat lres (Cons (w, Nil))
            | _ ->
                let lres = cart2l l in
                let rres = cart2l r in
                concat lres (concat (Cons (w, Nil)) rres)))
  | Leaf -> Nil

let fold_list f x w0 =
  let rec g x =
    match x with
    | Cons (h, t) -> f h (g t)
    | _ -> w0
  in
  g x

let max a b = if a < b then b else a

let append x y = fold_list (fun a b -> Cons (a, b)) x (Cons (y, Nil))

let fold f x w0 =
  let rec g x =
    match x with
    | Cons (h, t) -> f h (g t)
    | _ -> w0
  in
  g x

let length x = fold (fun a b -> b + 1) x 0

let sum x = fold (fun a b -> a + b) x 0

let head default l =
  match l with
  | Cons (h, t) -> h
  | Nil -> default

let minimum x = fold (fun a b -> if a < b then a else b) x (head 0 x)

let maximum x = fold (fun a b -> if a > b then a else b) x (head 0 x)

let raw_pre b =
  let rec f pre len rem =
    let sub_res =
      match rem with
      | Cons (h, t) -> f (append pre h) (len + 1) t
      | Nil -> 0
    in
    if b pre then max len sub_res else sub_res
  in
  fun rem -> f Nil 0 rem

let rec raw_suf b l =
  if b l then length l
  else
    match l with
    | Cons (h, t) -> raw_suf b t
    | _ -> 0

let rec raw_lsp b l =
  match l with
  | Cons (h, t) -> max (raw_pre b l) (raw_lsp b t)
  | Nil -> 0

let l2cart order =
  let insert w =
    let rec ins tmp p =
      match p with
      | ConsNode (dnode, rem) ->
          (match dnode with
           | Node (l, v, r) ->
               if order v w then ConsNode (Node (tmp, w, Leaf), p)
               else ins (Node (l, v, tmp)) rem)
      | NilNode -> ConsNode (Node (tmp, w, Leaf), NilNode)
    in
    fun p -> ins Leaf p
  in
  let merge =
    let rec go pre p =
      match p with
      | ConsNode (dnode, rem) ->
          (match dnode with
           | Node (l, v, r) -> go (Node (l, v, pre)) rem)
      | NilNode -> pre
    in
    fun p -> go Leaf p
  in
  let rec build p l =
    match l with
    | Cons (h, t) -> build (insert h p) t
    | Nil -> merge p
  in
  fun l -> build NilNode l

let rec last default l =
  match l with
  | Cons (h, t) ->
      (match t with
       | Nil -> h
       | _ -> last default t)
  | Nil -> default

let lmin l =
  match l with
  | Nil -> true
  | _ -> head 0 l = minimum l

let rmax l =
  match l with
  | Nil -> true
  | _ -> last 0 l = maximum l

let isval l = lmin l && rmax l

let order a b = b < a

let lsp r b x =
  let t = l2cart r x in
  raw_lsp b (cart2l t)

let run = lsp order isval