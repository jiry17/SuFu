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

fold_list = lambda f: Int->List->List. lambda x: List. lambda w0: List.  
  fix (lambda g: List->List. lambda x: List.
    match x with 
      cons {h, t} -> f h (g t)
    | _ -> w0
    end) x;

rev = lambda x: List. (fix (lambda f: List->List->List. lambda x: List. lambda y: List.
  match x with 
    cons {h, t} -> f t (cons {h, y})
  | _ -> y
  end
)) x (nil unit);

max = lambda x: Int. lambda y: Int. if (< x y) then y else x;
inf = 100;

/* Task */

/* Sketches for LSP */

lsp = lambda b: List->Bool. lambda x: List. ((fix (lambda f: List -> {Compress List, Int}. lambda x: List.
  match x with
    cons {h, t} -> let res = (f t) in
      let ms = res.2 in
        let x = res.1 in
          if (b (cons {h, x})) then {cons {h, x}, max ms (+ 1 (length x))}
          else (
            if (b (cons {h, nil unit})) then {cons {h, nil unit}, max ms 1}
            else {nil unit, ms}
          )
  | _ -> {nil unit, 0}
  end
)) x).2;

min = lambda x: Int. lambda y: Int. if (< x y) then x else y;
minimum = lambda x: List. fold (lambda h: Int. lambda t: Int. min h t) x inf;
isval = lambda x: List. 
  match x with
    cons {h, t} -> if (> h (minimum t)) then false else true
  | _ -> true
  end;


run = lsp isval;

/* tests  

l = cons {-3, cons {-2, cons {1, cons {3, nil unit}}}};
minimum l;
isval l;
run l;*/