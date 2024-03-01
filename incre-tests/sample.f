Inductive List = cons {Int, List} | nil Unit;

/* Library for lists */

fold = lambda f: Int->Int->Int. lambda x: List. lambda w0: Int.  
  fix (lambda g: List->Int. lambda x: List.
    match x with 
      cons {h, t} -> f h (g t)
    | _ -> w0
    end) x;

sum = lambda x: List. fold (lambda a: Int. lambda b: Int. (+ a b)) x 0;

fold_list = lambda f: Int->List->List. lambda x: List. lambda w0: List.  
  fix (lambda g: List->List. lambda x: List.
    match x with 
      cons {h, t} -> f h (g t)
    | _ -> w0
    end) x;

append = lambda x: List. lambda y: Int. fold_list (lambda a: Int. lambda b: List. cons{a, b}) x (cons {y, nil unit});

/* Task related functions */

test = fix (lambda f: List->List. lambda x:List.
    match x with 
      cons {h, t} ->
        let res = f t in
            if (> h 0) then cons {h, res} else (append res h) 
    | _ -> x
    end 
);

testsum = lambda x: List. sum (test x);

test_compress = fix (lambda f: List->Compress List. lambda x:List.
    match x with 
      cons {h, t} ->
        if (> h 0) then cons {h, f t} else append (f t) h
    | _ -> x
    end 
);

testsum_compress = lambda x: List. sum (test_compress x);

/* Tests 

l = cons {1, cons {-2, cons {3, cons {-1, nil unit}}}};
testsum l;
testsum_compress l;*/