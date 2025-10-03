Inductive AExpr = int Int | plus {AExpr, AExpr} | minus {AExpr, AExpr};
Inductive NExpr = nint Int | nplus {Int, NExpr, NExpr} | nminus {Int, NExpr, NExpr};

repr = fix (
  \f: NExpr -> AExpr. \e: NExpr.
  match e with
    nint a -> int a
  | nplus {_, a, b} -> plus {f a, f b}
  | nminus {_, a, b} -> minus {f a, f b}
  end
);

memo = \e: NExpr. match e with nint a -> a | nplus {a, _, _} -> a | nminus {a, _, _} -> a end;

is_memo = fix (
  \f: NExpr -> Bool. \e: NExpr.
  match e with
    nint a -> true
  | nplus {n, e1, e2} -> and (== n (+ (memo e1) (memo e2))) (and (f e1) (f e2))
  | nminus {n, e1, e2} -> and (== n (- (memo e1) (memo e2))) (and (f e1) (f e2))
  end
);

spec = fix (
  \f: AExpr -> Int. \e: AExpr.
  match e with
    int a -> a
  | plus {e1, e2} -> + (f e1) (f e2)
  | minus {e1, e2} -> - (f e1) (f e2)
  end
);

target = fix (
  \f: NExpr -> Compress NExpr. \e: NExpr.
  match e with
    nint a -> nint a
  | nplus {s, _, _} -> e /*Avoid recursions of sub-terms*/
  | nminus {s, _, _} -> e
  end
);

/*Customized generator*/
gen = fix (
  \f: AExpr -> NExpr. \e: AExpr.
  match e with
    int a -> nint a
  | plus {a, b} ->
    let res = {f a, f b} in nplus {+ (memo res.1) (memo res.2), res.1, res.2}
  | minus {a, b} ->
    let res = {f a, f b} in nminus {- (memo res.1) (memo res.2), res.1, res.2}
  end
);

main = \e: AExpr. let inp = gen e in
  if is_memo inp then spec (repr (target inp)) else 0;