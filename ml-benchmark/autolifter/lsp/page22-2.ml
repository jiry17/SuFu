type cart_tree = Node of cart_tree * int * cart_tree | Leaf of unit
type cart_path = ConsNode of cart_tree * cart_path | NilNode of unit
type list_ = Cons of int * list_ | Nil of unit

let l2cart order =
  let insert w =
    let rec insert tmp p =
      match p with
      | ConsNode (dnode, rem) ->
          (match dnode with
           | Node (l, v, r) ->
               if order v w
               then ConsNode (Node (tmp, w, Leaf ()), p)
               else insert (Node (l, v, tmp)) rem
           | _ -> p)
      | NilNode _ -> ConsNode (Node (tmp, w, Leaf ()), NilNode ())
    in
    insert
  in
  let merge0 =
    let rec f pre p =
      match p with
      | ConsNode (dnode, rem) ->
          (match dnode with
           | Node (l, v, r) -> f (Node (l, v, pre)) rem
           | _ -> pre)
      | NilNode _ -> pre
    in
    fun p -> f (Leaf ()) p
  in
  let rec build p l =
    match l with
    | Cons (h, t) ->
        let ins = insert h in
        build (ins p) t
    | Nil _ -> merge0 p
  in
  fun x -> build (NilNode ()) x

let rec concat x y =
  match x with
  | Cons (h, t) -> Cons (h, concat t y)
  | Nil _ -> y

let rec cart2l t =
  match t with
  | Node (l, w, r) ->
      (match l with
       | Leaf _ ->
           (match r with
            | Leaf _ -> Cons (w, Nil ())
            | Node (_, _, _) ->
                let rres = cart2l r in
                concat (Cons (w, Nil ())) rres)
       | Node (_, _, _) ->
           (match r with
            | Leaf _ ->
                let lres = cart2l l in
                concat lres (Cons (w, Nil ()))
            | Node (_, _, _) ->
                let lres = cart2l l in
                let rres = cart2l r in
                concat lres (concat (Cons (w, Nil ())) rres)))
  | Leaf _ -> Nil ()

let fold_list f x w0 =
  let rec g x =
    match x with
    | Cons (h, t) -> f h (g t)
    | _ -> w0
  in
  g x

let max a b = if a < b then b else a
let append x y = fold_list (fun a b -> Cons (a, b)) x (Cons (y, Nil ()))

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
  | Nil _ -> default

let minimum x = fold (fun a b -> if a < b then a else b) x (head 0 x)
let maximum x = fold (fun a b -> if a > b then a else b) x (head 0 x)

let raw_pre b =
  let rec f pre len rem =
    let sub_res =
      match rem with
      | Cons (h, t) ->
          let new_pre = append pre h in
          f new_pre (len + 1) t
      | Nil _ -> 0
    in
    if b pre then max len sub_res else sub_res
  in
  fun rem -> f (Nil ()) 0 rem

let raw_suf b =
  let rec f l =
    if b l then length l
    else
      match l with
      | Cons (h, t) -> f t
      | _ -> 0
  in
  f

let raw_lsp b =
  let rec f l =
    match l with
    | Cons (h, t) -> max (raw_pre b l) (f t)
    | Nil _ -> 0
  in
  f

let lsp r b x =
  let t = l2cart r x in
  raw_lsp b (cart2l t)

let last default =
  let rec f l =
    match l with
    | Cons (h, t) ->
        (match t with
         | Nil _ -> h
         | Cons (_, _) -> f t)
    | Nil _ -> default
  in
  f

let isval l =
  match l with
  | Nil _ -> true
  | _ -> (maximum l + minimum l) > length l

let order a b = b > a
let run = lsp order isval