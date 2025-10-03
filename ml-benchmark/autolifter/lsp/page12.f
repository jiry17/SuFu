/* Library for lists */

Inductive List = cons {Int, List} | nil Unit;

head = lambda x: List. lambda default: Int.
  match x with 
    cons {h, t} -> h
  | _ -> default
  end;

fold = lambda f: Int->Int->Int. lambda x: List. lambda w0: Int.  
  fix (lambda g: List->Int. lambda x: List.
    match x with 
      cons {h, t} -> f h (g t)
    | _ -> w0
    end) x;

length = lambda x: List. fold (lambda a: Int. lambda b: Int. + b 1) x 0;
sum = lambda x: List. fold (lambda a: Int. lambda b: Int. + a b) x 0;
lim = 10;

minimum = lambda x: List. fold (lambda a: Int. lambda b: Int. if (< a b) then a else b) x 0;

pushback = fix (
  \f: List->Int->List. \x: List. \a: Int.
  match x with
    cons {h, t} -> cons {h, f t a}
  | nil _ -> cons {a, nil unit}
  end
);

max = lambda a: Int. lambda b: Int. if (< a b) then b else a;

/* lsp template */

tail = \x: List.
  match x with
    cons {h, t} -> t
  | nil _ -> x
  end;

lsp = \pred: List -> Bool. \x: List. (fix (
  \f: List->List->Compress List->Int->Int. \l: List. \suf: List.
  \lpre: Compress List. lambda res: Int.
  let len = length lpre in
      if (or (== len 0) (pred lpre)) then
        match l with 
          cons {h, t} -> f t suf (pushback lpre h) (max res len)
        | nil _ -> max res len
        end
      else
        match suf with
          cons {h, t} -> f l t (tail lpre) res
      end
)) x x (nil unit) 0;

/*task related parts*/

isvalid = \x: List. (< (sum x) lim);

run = \x: List. if (< (minimum x) 0) then 0 else (lsp isvalid x);

/*l = cons {5, cons {3, cons {4, cons{1, nil unit}}}};
pushback l 3;
isvalid l;
run l;*/