Inductive ArithExpr = int Int | plus {ArithExpr, ArithExpr} | minus {ArithExpr, ArithExpr};
Inductive NormExpr = nint Int | nplus {NormExpr, NormExpr};

repr = fix (
  \f: NormExpr -> Compress ArithExpr. \e: NormExpr.
  match e with
    nint i -> int i
  | nplus {e1, e2} -> plus {f e1, f e2}
  end
);

spec = fix (
  \f: ArithExpr -> Int. \e: ArithExpr.
  match e with
    int i -> i
  | plus {e1, e2} -> + (f e1) (f e2)
  | minus {e1, e2} -> - (f e1) (f e2)
  end
);

main = \e: NormExpr. spec (repr e);