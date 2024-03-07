open Format
open Syntax
open Support.Error
open Support.Pervasive

(* ------------------------   EVALUATION  ------------------------ *)

exception NoRuleApplies

let rec isval ctx t = match t with
    TmTrue(_)  -> true
  | TmFalse(_) -> true
  | TmUnit(_)  -> true
  | TmInt _ -> true
  | TmAbs(_,_,_,_) -> true
  | TmTuple(_,fields) -> List.for_all (fun ti -> isval ctx ti) fields
  | TmConstructor(_,_,_,_) -> true
  | TmIndValue(_,_,_,_) -> true
  | TmLabel(_, t) -> isval ctx t
  | _ -> false

let rec eval1 ctx t = 
  match t with
    TmIf(_,TmTrue(_),t2,t3) ->
      t2
  | TmIf(_,TmFalse(_),t2,t3) ->
      t3
  | TmIf(fi,t1,t2,t3) ->
      let t1' = eval1 ctx t1 in
      TmIf(fi, t1', t2, t3)
  | TmVar(fi,n,_) ->
      (match getbinding fi ctx n with
          TmAbbBind(t,_) -> t 
        | _ -> raise NoRuleApplies)
  | TmPrimaryOp(fi, (_, _, _, f), ts) when (List.for_all (fun t -> isval ctx t) ts) ->
      f (fi, ts)
  | TmPrimaryOp(fi, op, ts) ->
      let rec f terms = (
          match terms with
        | h::t when isval ctx h -> h::(f t)
        | h::t -> (eval1 ctx h)::t
        | _ -> raise NoRuleApplies
      ) in TmPrimaryOp(fi, op, f ts)
  | TmLabel(fi, t) -> TmLabel(fi, eval1 ctx t)
  | TmLet(fi,x,v1,t2) when isval ctx v1 ->
      termSubstTop v1 t2 
  | TmLet(fi,x,t1,t2) ->
      let t1' = eval1 ctx t1 in
      TmLet(fi, x, t1', t2) 
  | TmTuple(fi,fields) ->
      let rec evalafield l = match l with 
        [] -> raise NoRuleApplies
      | vi::rest when isval ctx vi -> 
          let rest' = evalafield rest in
          vi::rest'
      | ti::rest -> 
          let ti' = eval1 ctx ti in
          ti'::rest
      in let fields' = evalafield fields in
      TmTuple(fi, fields')
  | TmProj(fi, TmTuple(_, fields), i) ->
      (try 
        List.nth fields (i - 1)
      with
        | Failure _ -> raise NoRuleApplies
        | Invalid_argument _ -> raise NoRuleApplies
      )
  | TmProj(fi, t1, l) ->
      let t1' = eval1 ctx t1 in
      TmProj(fi, t1', l)
  | TmApp(fi,TmAbs(_,x,tyT11,t12),v2) when isval ctx v2 ->
      termSubstTop v2 t12
  | TmApp(fi,TmConstructor(_,x,ty,_), v2) when isval ctx v2 ->
      TmIndValue(fi,x,v2,ty)
  | TmApp(fi,v1,t2) when isval ctx v1 ->
      let t2' = eval1 ctx t2 in
      TmApp(fi, v1, t2')
  | TmApp(fi,t1,t2) ->
      let t1' = eval1 ctx t1 in
      TmApp(fi, t1', t2)
  | TmFix(fi,v1) as t when isval ctx v1 ->
      (match v1 with
         TmAbs(_,_,_,t12) -> termSubstTop t t12
       | _ -> raise NoRuleApplies)
  | TmFix(fi,t1) ->
      let t1' = eval1 ctx t1
      in TmFix(fi,t1')
  | TmUnLabel(fi, t) when isval ctx t -> (
      match t with 
        TmLabel(_, t') -> t'
      | _ -> raise NoRuleApplies)
  | TmUnLabel (fi, t) -> 
      TmUnLabel (fi, eval1 ctx t)
  | TmAlign (fi, v) when isval ctx v -> v 
  | TmAlign (fi, t) ->
      TmAlign (fi, eval1 ctx t) 
  | TmMatch (fi, v, cases) when isval ctx v ->
      let rec ismatch pt v =
        ( match pt with
            PtUnderScore _ -> true
          | PtVar _ -> true
          | PtTuple (_, cases) -> 
            ( match v with 
                TmTuple (_, vs) -> 
                  ((List.length vs) = (List.length cases) &&
                  List.for_all (fun (sub_pt, sub_v) -> ismatch sub_pt sub_v) (List.combine cases vs))
              | _ -> false
            )
          | PtConstructor (_, cname, pt) ->
            ( match v with
                TmIndValue (_, vname, vs, _) -> cname = vname && (ismatch pt vs) 
              | _ -> false
            )
        ) in
          let (p, t) = List.find (fun (p, _) -> ismatch p v) cases in 
            let rec apply pt v t =
              (
                match pt with
                  PtUnderScore _ -> t
                | PtVar (fi, name) ->  let res = termSubstTop v t in res
                | PtConstructor (_, _, p) -> 
                  ( match v with 
                      TmIndValue(_, name, v', _) -> apply p v' t
                    | _ -> raise NoRuleApplies
                  )
                | PtTuple (_, ps) -> 
                  ( match v with
                      TmTuple(_, vs) -> List.fold_right (fun (p, v') -> fun st -> apply p v' st) (List.combine ps vs) t
                    | _ -> raise NoRuleApplies
                  )
              ) in 
                apply p v t
  | TmMatch (fi, t, cases) ->
    let t' = eval1 ctx t in TmMatch (fi, t', cases)
  | _ -> 
      raise NoRuleApplies

let rec eval ctx t =
  try let t' = eval1 ctx t
      in eval ctx t'
  with NoRuleApplies -> t

let evalbinding ctx b = match b with
    TmAbbBind(t,tyT) ->
      let t' = eval ctx t in 
      TmAbbBind(t',tyT)
  | bind -> bind

let istyabb ctx i = 
  match getbinding dummyinfo ctx i with
    TyAbbBind(tyT) -> true
  | _ -> false

let gettyabb ctx i = 
  match getbinding dummyinfo ctx i with
    TyAbbBind(tyT) -> tyT
  | _ -> raise NoRuleApplies

let rec computety ctx tyT = match tyT with
    TyVar(i,_) when istyabb ctx i -> gettyabb ctx i
  | _ -> raise NoRuleApplies

let rec simplifyty ctx tyT =
  try
    let tyT' = computety ctx tyT in
    simplifyty ctx tyT' 
  with NoRuleApplies -> tyT

let getid ctx ty =
  match ty with 
    TyVar (x, _) -> x 
  | _ -> -1

let rec tyeqv' ctxs ctxt ityS ityT =
  let sid = getid ctxs ityS in 
  let tid = getid ctxt ityT in
  if sid = tid && sid >= 0 then true else 
  let tyS = simplifyty ctxs ityS in
  let tyT = simplifyty ctxt ityT in
  (*pr "\neq\n"; printty ctxs tyS; pr "\n\n"; printty ctxt tyT; pr "\n";*)
  match (tyS,tyT) with
    (TyUnit,TyUnit) -> true
  | (TyId(b1),TyId(b2)) -> b1=b2
  | (TyArr(tyS1,tyS2),TyArr(tyT1,tyT2)) ->
       (tyeqv' ctxs ctxt tyS1 tyT1) && (tyeqv' ctxs ctxt tyS2 tyT2)
  | (TyBool,TyBool) -> true
  | (TyInt,TyInt) -> true
  | (TyTuple(fields1),TyTuple(fields2)) -> 
       List.length fields1 = List.length fields2
       &&                                         
       List.for_all 
         (fun (ty1, ty2) -> tyeqv' ctxs ctxt ty1 ty2)
         (List.combine fields1 fields2) 
  | (TyInductive(name1, cx), TyInductive(name2, cy)) ->
      if List.length cx <> List.length cy then false else
        let ctxs' = addbinding ctxs name1 (TyAbbBind ityS) in
        let ctxt' = addbinding ctxt name2 (TyAbbBind ityT) in 
          List.for_all
            (fun ((cnamex, tyx), (cnamey, tyy)) -> cnamex = cnamey && tyeqv' ctxs' ctxt' tyx tyy)
          (List.combine cx cy)
  | (TyCompress(t1), TyCompress(t2)) -> tyeqv' ctxs ctxt t1 t2
  | _ -> false

let tyeqv ctx tx ty = tyeqv' ctx ctx tx ty 

(* ------------------------   TYPING  ------------------------ *)

let rec typeof ctx t = 
  match t with
    TmTrue(fi) -> 
      TyBool
  | TmFalse(fi) -> 
      TyBool
  | TmIf(fi,t1,t2,t3) ->
     if tyeqv ctx (typeof ctx t1) TyBool then
       let tyT2 = typeof ctx t2 in
       if tyeqv ctx tyT2 (typeof ctx t3) then tyT2
       else error fi "arms of conditional have different types"
     else error fi "guard of conditional not a boolean"
  | TmUnit(fi) -> TyUnit
  | TmInt _  -> TyInt
  | TmPrimaryOp(fi, (name, itys, oty, _), ts) ->
    let ct = List.combine itys ts in 
      if (List.for_all (fun (ty, term) -> tyeqv ctx (typeof ctx term) ty) ct) then
        oty
      else error fi ("types of parameters do not match")
  | TmVar(fi,i,_) -> getTypeFromContext fi ctx i 
  | TmLabel (fi, t) -> TyCompress (typeof ctx t)
  | TmLet(fi,x,t1,t2) ->
     let tyT1 = typeof ctx t1 in
     let ctx' = addbinding ctx x (VarBind(tyT1)) in         
     typeShift (-1) (typeof ctx' t2)
  | TmTuple(fi, fields) ->
      let fieldtys = 
        List.map (fun ti -> typeof ctx ti) fields in
      TyTuple(fieldtys)
  | TmProj(fi, t1, l) ->
      (match simplifyty ctx (typeof ctx t1) with
          TyTuple(fieldtys) ->
            ( try
                List.nth fieldtys (l - 1)
              with
              | Failure _ -> error fi ("field "^string_of_int(l)^" not found")
              | Invalid_argument _ -> error fi ("field "^string_of_int(l)^" not found")
            )
        | _ -> error fi "Expected record type")
  | TmAbs(fi,x,tyT1,t2) ->
      let ctx' = addbinding ctx x (VarBind(tyT1)) in
      let tyT2 = typeof ctx' t2 in
      TyArr(tyT1, typeShift (-1) tyT2)
  | TmApp(fi,t1,t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match simplifyty ctx tyT1 with
          TyArr(tyT11,tyT12) ->
            if tyeqv ctx tyT2 tyT11 then tyT12
            else error fi "parameter type mismatch"
        | _ -> error fi "arrow type expected")
  | TmFix(fi, t1) ->
      let tyT1 = typeof ctx t1 in
      (match simplifyty ctx tyT1 with
           TyArr(tyT11,tyT12) ->
             if tyeqv ctx tyT12 tyT11 then tyT12
             else error fi "result of body not compatible with domain"
         | _ ->  error fi "arrow type expected")
  | TmConstructor(fi, _, t1, t2) -> TyArr(t2, t1)
  | TmIndValue(fi, _, _, t) -> t 
  | TmUnLabel(fi, t) ->
      let ct' = typeof ctx t in (
        match (simplifyty ctx ct') with
          TyCompress cty -> cty
        | _ -> error fi "content of unlabel much return compressed values"
      )
  | TmAlign(fi, t) -> typeof ctx t
  | TmMatch(fi, t1, cases) ->
    if List.length cases = 0 then
      error fi "match expression should have at least one case"
    else let ty1 = simplifyty ctx (typeof ctx t1) in
      let rec addptbinding current_ctx ty pt =
        ( match pt with 
          PtUnderScore _ -> current_ctx 
        | PtTuple (fi, cases) ->
          ( match (getproduct ty) with
            None -> error fi "pattern does not match the type product"
          | Some cs -> if (List.length cs) <> (List.length cases) then
              error fi "pattern does not match the type product"
            else let (si, res_ctx) = List.fold_left 
              (fun (si, pctx) -> fun (case, cty) -> (si + (ptsize case), addptbinding pctx (typeShift si cty) case))
              (0, current_ctx) (List.combine cases cs) in res_ctx
          )
        | PtVar (fi, name) -> addbinding current_ctx name (VarBind(ty))
        | PtConstructor (fi, name, p) ->
          ( 
            match (getconstructty (simplifyty current_ctx ty) name) with 
              None -> error fi "pattern does not match the type constructor"
            | Some cty -> addptbinding current_ctx cty p
          )
        ) in
        let deal_case (pt, t) = 
          (let ctx' = addptbinding ctx ty1 pt in 
            let tty = typeof ctx' t in typeShift (-(ptsize pt)) tty) in 
          let ty_list = List.map deal_case cases in
            let res_ty = List.hd ty_list in
              List.iter (fun cty -> if Bool.not (tyeqv ctx res_ty cty) then error fi "case types are not consistent") ty_list;
              res_ty