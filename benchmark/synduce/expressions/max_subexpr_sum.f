Inductive ArithExpr = int Int | plus {ArithExpr, ArithExpr} | minus {ArithExpr, ArithExpr};
Inductive PsumExpr = nint Int | nplus {PsumExpr, PsumExpr} | nminus {PsumExpr, PsumExpr};

max = \a: Int. \b: Int. if < a b then b else a;

repr = fix (
  \f: PsumExpr -> Compress ArithExpr. \e: PsumExpr.
  match e with
    nint i -> int i
  | nplus {e1, e2} -> plus {f e1, f e2}
  | nminus {e1, e2} -> minus {f e1, f e2}
  end
);

spec = \e: ArithExpr. (fix (
  \f: ArithExpr -> {Int, Int}. \e: ArithExpr.
  match e with
    int i -> {i,i}
  | plus {e1, e2} ->
    let r1 = f e1 in
    let r2 = f e2 in
    let sum = + r1.1 r2.1 in
    let m = max sum (max r1.2 r2.2) in
    {sum, m}
  | minus {e1, e2} ->
    let r1 = f e1 in
    let r2 = f e2 in
    let sum = - r1.1 r2.1 in
    let m = max sum (max r1.2 r2.2) in
    {sum, m}
  end
) e).2;

main = \e: PsumExpr. spec (repr e);