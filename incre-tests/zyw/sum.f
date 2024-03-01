
Inductive List = nil Unit | cons {Int, List};

sum = fix (
    \f: List -> Int. \xs: List. 
    match xs with
      nil _ -> 0
    | cons {h, t} -> + h (f t)
    end
);

spec = \xs: List. 
    sum xs;

repr = fix (
    \f: List -> Compress List. \xs: List. 
    match xs with
      nil _ -> 
        let tmp1 = (nil unit) in 
            align (label tmp1 ) 
    | cons {h, t} -> 
        let tmp2 = (f t) in 
            align (label (cons {h, unlabel tmp2 }) ) 
    end
);

main = \xs: List. 
    let tmp3 = (repr xs) in 
        align (spec (unlabel tmp3 )) ;
