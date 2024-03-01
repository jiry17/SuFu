Inductive ListR = consR {Int, ListR} | nilR Unit;
Inductive ListL = consL {ListL, Int} | nilL Unit;

ltor = fix (\f: ListL -> ListR. \l: ListL.
  match l with 
    consL {t, h} -> consR {h, f t}
  | nilL _ -> nilR unit
  end
);

rtol = fix (\f: ListR -> ListL. \l: ListR.
  match l with
    consR {h, t} -> consL {f t, h}
  | nilR _ -> nilL unit
  end
);

rec = fix (\f: ListL -> {Compress ListL, Compress ListR}. \l: ListL.
  match l with 
    consL {t, h} -> let res = f t in
      {consL {rtol res.2, h}, ltor res.1}
  | _ -> {nilL unit, nilR unit}
  end
);

sumL = fix (
  \f: ListL -> Int. \l: ListL.
  match l with
    consL {t, h} -> + h (f t)
  | nilL _ -> 0
  end
);

sumR = fix (
  \f: ListR -> Int. \l: ListR.
  match l with
    consR {h, t} -> + h (f t)
  | nilR _ -> 0
  end
);

run = \l: ListL. let res = rec l in {sumL res.1, sumR res.2};