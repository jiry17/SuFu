Config ClauseNum = 2;

Inductive List = cons {Int, List} | nil Unit;
Inductive CartTree = node {CartTree, Int, CartTree} | leaf Unit;
Inductive CartPath = consNode {CartTree, CartPath} | nilNode Unit;

l2cart = \order: Int -> Int -> Bool.
  let insert = \w: Int. (fix (
    \insert: CartTree -> CartPath -> CartPath. \tmp: CartTree. \p: CartPath.
    match p with
      consNode {dnode, rem} -> (
        match dnode with
          node {l, v, r} ->
            if (order v w) then consNode {node {tmp, w, leaf unit}, p}
            else insert (node {l, v, tmp}) rem
        end
      )
    | _ -> consNode {node {tmp, w, leaf unit}, nilNode unit}
    end
  )) (leaf unit) in
  let merge = (fix ( \f: CartTree -> CartPath -> CartTree. \pre: CartTree. \p: CartPath.
    match p with
      consNode {dnode, rem} -> (
        match dnode with
          node {l, v, r} -> f (node {l, v, pre}) rem
        end
      )
    | _ -> pre
    end
  )) (leaf unit) in
  (fix (
    \f: CartPath -> List -> CartTree. \p: CartPath. \l: List.
    match l with
      cons {h, t} -> f (insert h p) t
    | nil _ -> merge p
    end
  )) (nilNode unit);


concat = fix (lambda f: List->List->List. lambda x: List. lambda y: List.
  match x with
    cons {h, t} -> cons {h, f t y}
  | nil _ -> y
  end
);

cart2l = fix (
  \f: CartTree -> Compress List. \t: CartTree.
  match t with
    node {leaf _, w, leaf _} -> cons {w, nil unit}
  | node {l, w, leaf _} ->
    let lres = f l in
      concat lres (cons {w, nil unit})
  | node {leaf _, w, r} ->
    let rres = f r in
      concat (cons {w, nil unit}) rres
  | node {l, w, r} ->
      let lres = f l in
        let rres = f r in
          concat lres (concat (cons {w, nil unit}) rres)
  | _ -> nil unit
  end
);

fold_list = lambda f: Int->List->List. lambda x: List. lambda w0: List.
  fix (lambda g: List->List. lambda x: List.
    match x with
      cons {h, t} -> f h (g t)
    | _ -> w0
    end) x;

max = \a: Int. \b: Int. if (< a b) then b else a;
append = lambda x: List. lambda y: Int. fold_list (lambda a: Int. lambda b: List. cons{a, b}) x (cons {y, nil unit});


fold = lambda f: Int->Int->Int. lambda x: List. lambda w0: Int.
  fix (lambda g: List->Int. lambda x: List.
    match x with
      cons {h, t} -> f h (g t)
    | _ -> w0
    end) x;

length = lambda x: List. fold (lambda a: Int. lambda b: Int. + b 1) x 0;
sum = lambda x: List. fold (lambda a: Int. lambda b: Int. + a b) x 0;
head = lambda default: Int. lambda l: List.
  match l with
    cons {h, t} -> h
  | nil _ -> default
  end;

minimum = lambda x: List. fold (lambda a: Int. lambda b: Int. if (< a b) then a else b) x (head 0 x);
maximum = lambda x: List. fold (lambda a: Int. lambda b: Int. if (> a b) then a else b) x (head 0 x);

raw_pre = \b: List -> Bool.
  (fix (\f: List->Int->List->Int. \pre: List. \len: Int. \rem: List.
    let sub_res = match rem with
      cons {h, t} -> f (append pre h) (+ len 1) t
    | nil _ -> 0
    end in
      if (b pre) then max len sub_res
      else sub_res
  )) (nil unit) 0;

raw_lsp = \b: List -> Bool.
  fix (\f: List -> Int. \l: List.
    match l with
      cons {h, t} -> max (raw_pre b l) (f t)
    | nil _ -> 0
    end
);

lsp = \r: Int->Int->Bool. \b: List->Bool. \x: List.
  let t = l2cart r x in
    raw_lsp b (cart2l t);


last = lambda default: Int. fix (
  \f: List -> Int. \l: List.
  match l with
    cons {h, nil _} -> h
  | cons {h, t} -> f t
  | nil _ -> default
  end
);

isval = \l: List.
  match l with
    nil _ -> true
  | _ -> < (+ (maximum l) (minimum l)) (length l)
  end;

order = \a: Int. \b: Int. (< b a);

run = lsp order isval;