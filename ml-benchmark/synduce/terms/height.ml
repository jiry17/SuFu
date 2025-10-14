type arithOp = APlus of unit | AMinus of unit | AGt of unit
type boolOp = BNot of unit | BAnd of unit | BOr of unit | BEq of unit
type term =
  | TArithBin of arithOp * term * term
  | TBoolBin of boolOp * term * term
  | TArithUn of arithOp * term
  | TBoolUn of boolOp * term
  | TVar of int
  | TCInt of int
  | TCBool of bool

type op = OpPlus of unit | OpMinus of unit | OpNot of unit | OpAnd of unit | OpOr of unit | OpGt of unit | OpEq of unit
type term2 = Bin of op * term2 * term2 | Un of op * term2 | Var of int | CInt of int | CBool of bool

let mk_bin t1 t2 op =
  match op with
  | OpPlus _ -> TArithBin (APlus (), t1, t2)
  | OpMinus _ -> TArithBin (AMinus (), t1, t2)
  | OpNot _ -> TBoolBin (BNot (), t1, t2)
  | OpAnd _ -> TBoolBin (BAnd (), t1, t2)
  | OpOr _ -> TBoolBin (BOr (), t1, t2)
  | OpGt _ -> TArithBin (AGt (), t1, t2)
  | OpEq _ -> TBoolBin (BEq (), t1, t2)

let mk_un t op =
  match op with
  | OpPlus _ -> TArithUn (APlus (), t)
  | OpMinus _ -> TArithUn (AMinus (), t)
  | OpNot _ -> TBoolUn (BNot (), t)
  | OpAnd _ -> TBoolUn (BAnd (), t)
  | OpOr _ -> TBoolUn (BOr (), t)
  | OpGt _ -> TArithUn (AGt (), t)
  | OpEq _ -> TBoolUn (BEq (), t)

let rec repr t =
  match t with
  | Bin (op, t1, t2) -> mk_bin (repr t1) (repr t2) op
  | Un (o, x) -> mk_un (repr x) o
  | Var i -> TVar i
  | CInt i -> TCInt i
  | CBool b -> TCBool b

let max a b =
  if a < b then b else a

let rec spec t =
  match t with
  | TArithBin (op, t1, t2) -> 1 + (max (spec t1) (spec t2))
  | TBoolBin (op, t1, t2) -> 1 + (max (spec t1) (spec t2))
  | TArithUn (op, t) -> 1 + (spec t)
  | TBoolUn (op, t) -> 1 + (spec t)
  | TVar _ -> 1
  | TCInt _ -> 1
  | TCBool _ -> 1

let rec target t =
  match t with
  | Bin (op, t1, t2) -> Bin (op, target t1, target t2)
  | Un (op, t) -> Un (op, target t)
  | Var i -> Var i
  | CInt i -> CInt i
  | CBool i -> CBool i

let program xs = spec (repr (target xs))