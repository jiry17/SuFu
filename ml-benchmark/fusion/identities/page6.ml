type list_ =
  | Nil of unit
  | Cons of { h: int; t: list_ }

type list2d =
  | Nil2D of unit
  | Cons2D of { h: list_; t: list2d }

type list3d =
  | Nil3D of unit
  | Cons3D of { h: list2d; t: list3d }

let head xs =
  match xs with
  | Nil2D _ -> Nil ()
  | Cons2D r -> r.h

let rec tails xs =
  match xs with
  | Nil _ -> Cons2D { h = xs; t = Nil2D () }
  | Cons r -> Cons2D { h = xs; t = tails r.t }

let append w =
  let rec f xs =
    match xs with
    | Nil _ -> Cons { h = w; t = Nil () }
    | Cons r -> Cons { h = r.h; t = f r.t }
  in
  f

let scanl f init =
  let rec g now xs =
    match xs with
    | Nil _ -> Cons2D { h = now; t = Nil2D () }
    | Cons r -> Cons2D { h = now; t = g (f r.h now) r.t }
  in
  g init

let inits = scanl append (Nil ())

let map g =
  let rec f xs =
    match xs with
    | Nil2D _ -> Nil3D ()
    | Cons2D r -> Cons3D { h = g r.h; t = f r.t }
  in
  f

let rec concat xs =
  match xs with
  | Nil3D _ -> Nil2D ()
  | Cons3D r ->
      let rec g ys =
        match ys with
        | Nil2D _ -> concat r.t
        | Cons2D r1 -> Cons2D { h = r1.h; t = g r1.t }
      in
      g r.h

let segs xs = concat (map inits (tails xs))

let mapL g =
  let rec f xs =
    match xs with
    | Nil2D _ -> Nil ()
    | Cons2D r -> Cons { h = g r.h; t = f r.t }
  in
  f

let rec sum xs =
  match xs with
  | Nil _ -> 0
  | Cons r -> r.h + sum r.t

let max a b = if a < b then b else a

let rec maximum xs =
  match xs with
  | Nil _ -> 0
  | Cons r -> max r.h (maximum r.t)

let maxsum xs = maximum (mapL sum xs)

let mss xs = maxsum (segs xs)