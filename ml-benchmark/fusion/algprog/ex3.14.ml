type tree = Tip of int | Bin of tree * tree
type ptree = PTip of int * int | PBin of ptree * ptree

let rec tri t =
  match t with
  | Tip w -> PTip (0, w)
  | Bin (l, r) ->
      let rec step ys =
        match ys with
        | PTip (n, w) -> PTip (n + 1, w)
        | PBin (l2, r2) ->
            let l3 = step l2 in
            let r3 = step r2 in
            PBin (l3, r3)
      in
      let l1 = tri l in
      let r1 = tri r in
      PBin (step l1, step r1)

let rec tsum t =
  match t with
  | PTip (n, w) -> n * w
  | PBin (l, r) ->
      let sl = tsum l in
      let sr = tsum r in
      sl + sr

let program t = tsum (tri t)