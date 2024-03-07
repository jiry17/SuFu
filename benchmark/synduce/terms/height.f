Inductive ArithOp = aPlus Unit | aMinus Unit | aGt Unit;
Inductive BoolOp = bNot Unit | bAnd Unit | bOr Unit | bEq Unit;
Inductive Term =
  tArithBin {ArithOp, Term, Term}
| tBoolBin {BoolOp, Term, Term}
| tArithUn {ArithOp, Term}
| tBoolUn {BoolOp, Term}
| tVar Int | tCInt Int | tCBool Bool;

Inductive Op = opPlus Unit | opMinus Unit | opNot Unit | opAnd Unit | opOr Unit | opGt Unit | opEq Unit;
Inductive Term2 =
  bin {Op, Term2, Term2}
| un {Op, Term2}
| var Int | cInt Int | cBool Bool;

mk_bin = \t1: Term. \t2: Term. \op: Op.
match op with
  opPlus _ -> tArithBin {aPlus unit, t1, t2}
| opMinus _ -> tArithBin {aMinus unit, t1, t2}
| opNot _ -> tBoolBin {bNot unit, t1, t2}
| opAnd _ -> tBoolBin {bAnd unit, t1, t2}
| opOr _ -> tBoolBin {bOr unit, t1, t2}
| opGt _ -> tArithBin {aGt unit, t1, t2}
| opEq _ -> tBoolBin {bEq unit, t1, t2}
end;

mk_un = \t: Term. \op: Op.
match op with
  opPlus _ -> tArithUn {aPlus unit, t}
| opMinus _ -> tArithUn {aMinus unit, t}
| opNot _ -> tBoolUn {bNot unit, t}
| opAnd _ -> tBoolUn {bAnd unit, t}
| opOr _ -> tBoolUn {bOr unit, t}
| opGt _ -> tArithUn {aGt unit, t}
| opEq _ -> tBoolUn {bEq unit, t}
end;

repr = fix (
  \f: Term2 -> Term. \t: Term2.
  match t with
    bin {op, t1, t2} -> mk_bin (f t1) (f t2) op
  | un {o, x} -> mk_un (f x) o
  | var i -> tVar i
  | cInt i -> tCInt i
  | cBool b -> tCBool b
  end
);

max = \a: Int. \b: Int. if < a b then b else a;

spec = fix (
  \f: Term -> Int. \t: Term.
  match t with
    tArithBin {op, t1, t2} -> + 1 (max (f t1) (f t2))
  | tBoolBin {op, t1, t2} -> + 1 (max (f t1) (f t2))
  | tArithUn {op, t} -> + 1 (f t)
  | tBoolUn {op, t} -> + 1 (f t)
  | tVar _ -> 1
  | tCInt _ -> 1
  | tCBool _ -> 1
  end
);

target = fix (
  \f: Term2 -> Compress Term2. \t: Term2.
  match t with
    bin {op, t1, t2} -> bin {op, f t1, f t2}
  | un {op, t} -> un {op, f t}
  | var i -> var i
  | cInt i -> cInt i
  | cBool i -> cBool i
  end
);

main = \xs: Term2. spec (repr (target xs));