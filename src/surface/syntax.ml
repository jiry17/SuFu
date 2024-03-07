open Format
open Support.Error
open Support.Pervasive

(* ---------------------------------------------------------------------- *)
(* Datatypes *)

type ty =
    TyId of string
  | TyVar of int * int
  | TyUnit
  | TyTuple of ty list
  | TyBool
  | TyInt
  | TyArr of ty * ty
  | TyInductive of (string) * (string * ty) list 
  | TyCompress of ty

type pattern =
    PtUnderScore of info 
  | PtVar of info * string 
  | PtTuple of info * pattern list 
  | PtConstructor of info * string * pattern 

type term =
  TmTrue of info
| TmFalse of info
| TmIf of info * term * term * term
| TmUnit of info
| TmVar of info * int * int
| TmInt of info * int
| TmPrimaryOp of info * primaryop * term list
| TmLet of info * string * term * term
| TmTuple of info * term list
| TmProj of info * term * int
| TmAbs of info * string * ty * term
| TmApp of info * term * term
| TmFix of info * term
| TmConstructor of info * string * ty * ty
| TmIndValue of info * string * term * ty
| TmMatch of info * term * (pattern * term) list 
| TmAlign of info * term
| TmLabel of info * term
| TmUnLabel of info * term
and primaryop = string * ty list * ty * ((info * term list) -> term)


type binding =
    NameBind 
  | TmAbbBind of term * (ty option)
  | VarBind of ty
  | TyVarBind
  | TyAbbBind of ty

type context = (string * binding) list

type config_value =
  BoolConfig of bool
| IntConfig of int
| StringConfig of string

type command =
  Import of string
| Config of string * config_value
| Eval of info * term
| Bind of info * string * binding
| DefInductive of info * string * (string * ty) list


(* ---------------------------------------------------------------------- *)
(* Context management *)

let emptycontext = []

let ctx2string ctx = (List.fold_left (fun s (x,_) -> s ^ " " ^ x) "" ctx)

let ctxlength ctx = List.length ctx

let addbinding ctx x bind = (x,bind)::ctx

let addname ctx x = addbinding ctx x NameBind

let rec addpattern ctx pattern =
  match pattern with 
    PtUnderScore _ -> ctx
  | PtConstructor(_, _, p) -> addpattern ctx p
  | PtVar(_, name) -> addbinding ctx name NameBind
  | PtTuple(_, plist) -> List.fold_left (fun pctx -> fun p -> addpattern pctx p) ctx plist

let rec isnamebound ctx x =
  match ctx with
      [] -> false
    | (y,_)::rest ->
        if y=x then true
        else isnamebound rest x

let rec pickfreshname ctx x =
  if isnamebound ctx x then pickfreshname ctx (x^"'")
  else ((x,NameBind)::ctx), x

let index2name fi ctx x =
  try
    let (xn,_) = List.nth ctx x in
    xn
  with Failure _ -> 
    let msg =
      Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d" in
    error fi (msg x (List.length ctx))

let rec name2index fi ctx x =
  match ctx with
      [] -> error fi ("Identifier " ^ x ^ " is unbound")
    | (y,_)::rest ->
        if y=x then 0
        else 1 + (name2index fi rest x)

(* ---------------------------------------------------------------------- *)
(* Shifting *)

let tymap onvar c tyT = 
  let rec walk c tyT = match tyT with
    TyId(b) as tyT -> tyT
  | TyUnit -> TyUnit
  | TyTuple(fields) -> TyTuple(List.map (fun tyTi -> walk c tyTi) fields)
  | TyVar(x,n) -> let res = onvar c x n in res
  | TyArr(tyT1,tyT2) -> TyArr(walk c tyT1,walk c tyT2)
  | TyBool -> TyBool
  | TyInt -> TyInt
  | TyInductive(x, cons_list) ->
      let cons_list' = List.map (fun (name, ty) -> (name, walk (c + 1) ty)) cons_list in
        TyInductive(x, cons_list')
  | TyCompress(t) -> TyCompress (walk c t)
  in walk c tyT

let rec ptsize pt =
  match pt with
    PtUnderScore _ -> 0
  | PtConstructor (_, name, p1) -> ptsize p1
  | PtTuple (_, cs) -> List.fold_left (fun w -> fun p -> w + (ptsize p)) 0 cs
  | PtVar (_, _) -> 1

let tmmap onvar ontype c t = 
  let rec walk c t = match t with
    TmVar(fi,x,n) -> onvar fi c x n
  | TmTrue(fi) as t -> t
  | TmFalse(fi) as t -> t
  | TmProj(fi, t, x) -> TmProj(fi, walk c t, x)
  | TmIf(fi,t1,t2,t3) -> TmIf(fi,walk c t1,walk c t2,walk c t3)
  | TmLet(fi,x,t1,t2) -> TmLet(fi,x,walk c t1,walk (c+1) t2)
  | TmUnit(fi) as t -> t
  | TmInt _ as t -> t
  | TmPrimaryOp(fi,op,ts) -> TmPrimaryOp(fi, op, List.map (walk c) ts)
  | TmTuple(fi,fields) -> TmTuple(fi,List.map (fun ti -> walk c ti) fields)
  | TmAbs(fi,x,tyT1,t2) -> TmAbs(fi,x,ontype c tyT1,walk (c+1) t2)
  | TmApp(fi,t1,t2) -> TmApp(fi,walk c t1,walk c t2)
  | TmFix(fi,t1) -> TmFix(fi,walk c t1)
  | TmConstructor(fi,name,ty1,ty2) -> TmConstructor(fi, name, ontype c ty1, ontype c ty2) 
  | TmIndValue(fi, name, t1, ty1) -> TmIndValue(fi, name, walk c t1, ontype c ty1)
  | TmMatch(fi, t, cases) -> 
      TmMatch(fi, walk c t, List.map (fun (p, t) -> (p, walk (c + (ptsize p)) t)) cases)
  | TmAlign(fi, t) -> TmAlign(fi, walk c t)
  | TmLabel(fi, t) -> TmLabel(fi, walk c t)
  | TmUnLabel(fi, t) -> TmUnLabel(fi, walk c t)
  in walk c t

let typeShiftAbove d c tyT =
  tymap
    (fun c x n -> if x>=c then TyVar(x+d,n+d) else TyVar(x,n+d))
    c tyT

let termShiftAbove d c t =
  tmmap
    (fun fi c x n -> if x>=c then TmVar(fi,x+d,n+d) 
                     else TmVar(fi,x,n+d))
    (typeShiftAbove d)
    c t

let termShift d t = termShiftAbove d 0 t

let typeShift d tyT = typeShiftAbove d 0 tyT

let bindingshift d bind =
  match bind with
    NameBind -> NameBind
  | TmAbbBind(t,tyT_opt) ->
     let tyT_opt' = match tyT_opt with
                      None->None
                    | Some(tyT) -> Some(typeShift d tyT) in
     TmAbbBind(termShift d t, tyT_opt')
  | VarBind(tyT) -> VarBind(typeShift d tyT)
  | TyVarBind -> TyVarBind
  | TyAbbBind(tyT) -> TyAbbBind(typeShift d tyT)

(* ---------------------------------------------------------------------- *)
(* Substitution *)

let termSubst j s t =
  tmmap
    (fun fi j x n -> if x=j then termShift j s else TmVar(fi,x,n))
    (fun j tyT -> tyT)
    j t

let termSubstTop s t = 
  termShift (-1) (termSubst 0 (termShift 1 s) t)

let typeSubst tyS j tyT =
  tymap
    (fun j x n -> if x=j then (typeShift j tyS) else (TyVar(x,n)))
    j tyT

let typeSubstTop tyS tyT = 
  typeShift (-1) (typeSubst (typeShift 1 tyS) 0 tyT)

let rec tytermSubst tyS j t =
  tmmap (fun fi c x n -> TmVar(fi,x,n))
        (fun j tyT -> typeSubst tyS j tyT) j t

let tytermSubstTop tyS t = 
  termShift (-1) (tytermSubst (typeShift 1 tyS) 0 t)
  
let getproduct ty = 
    match ty with 
      TyTuple cs -> Some cs
    | _ -> None
  
let getconstructty ty name =
      match ty with
        TyInductive (_, cons_list) ->
          (try 
            (let (cname, cty) = List.find (fun (cname, _) -> cname = name) cons_list 
              in Some (typeSubstTop ty cty))
          with Not_found -> None)
      | _ -> None

(* ---------------------------------------------------------------------- *)
(* Context management (continued) *)

let rec getbinding fi ctx i =
  try
    let (_,bind) = List.nth ctx i in
    bindingshift (i+1) bind 
  with Failure _ -> 
    let msg =
      Printf.sprintf "Variable lookup failure: offset: %d, ctx size: %d" in
    error fi (msg i (List.length ctx))
 let getTypeFromContext fi ctx i =
   match getbinding fi ctx i with
         VarBind(tyT) -> tyT
     | TmAbbBind(_,Some(tyT)) -> tyT
     | TmAbbBind(_,None) -> error fi ("No type recorded for variable "
                                        ^ (index2name fi ctx i))
     | _ -> error fi 
       ("getTypeFromContext: Wrong kind of binding for variable " 
         ^ (index2name fi ctx i)) 
(* ---------------------------------------------------------------------- *)
(* Extracting file info *)

let tmInfo t = match t with
    TmTrue(fi) -> fi
  | TmFalse(fi) -> fi
  | TmIf(fi,_,_,_) -> fi
  | TmUnit(fi) -> fi
  | TmVar(fi,_,_) -> fi
  | TmInt(fi,_) -> fi
  | TmPrimaryOp(fi,_,_) ->fi
  | TmLet(fi,_,_,_) -> fi
  | TmProj(fi,_,_) -> fi
  | TmTuple(fi,_) -> fi
  | TmAbs(fi,_,_,_) -> fi
  | TmApp(fi, _, _) -> fi
  | TmFix(fi,_) -> fi
  | TmIndValue(fi, _, _, _) -> fi
  | TmConstructor(fi, _, _, _) -> fi
  | TmMatch(fi, _, _) -> fi
  | TmAlign(fi, _) -> fi
  | TmLabel(fi, _) -> fi 
  | TmUnLabel(fi, _) -> fi

(* ---------------------------------------------------------------------- *)
(* Printing *)

(* The printing functions call these utility functions to insert grouping
  information and line-breaking hints for the pretty-printing library:
     obox   Open a "box" whose contents will be indented by two spaces if
            the whole box cannot fit on the current line
     obox0  Same but indent continuation lines to the same column as the
            beginning of the box rather than 2 more columns to the right
     cbox   Close the current box
     break  Insert a breakpoint indicating where the line maybe broken if
            necessary.
  See the documentation for the Format module in the OCaml library for
  more details. 
*)

let obox0() = open_hvbox 0
let obox() = open_hvbox 2
let cbox() = close_box()
let break() = print_break 0 0

let small t = 
  match t with
    TmVar(_,_,_) -> true
  | _ -> false

let rec printty_Type outer ctx tyT = match tyT with
      tyT -> printty_ArrowType outer ctx tyT

and printty_ArrowType outer ctx  tyT = match tyT with 
    TyArr(tyT1,tyT2) ->
      obox0(); 
      printty_AType false ctx tyT1;
      if outer then pr " ";
      pr "->";
      if outer then print_space() else break();
      printty_ArrowType outer ctx tyT2;
      cbox()
  | tyT -> printty_AType outer ctx tyT

and printty_AType outer ctx tyT = match tyT with
    TyBool -> pr "Bool"
  | TyUnit -> pr "Unit"
  | TyId(b) -> pr b
  | TyTuple(fields) ->
        let rec p l = match l with 
            [] -> ()
          | [ty] -> printty_Type false ctx ty
          | ty::rest ->
              printty_Type false ctx ty; pr","; if outer then print_space() else break(); 
              p rest
        in pr "{"; open_hovbox 0; p fields; pr "}"; cbox()
  | TyVar(x,n) ->
      if ctxlength ctx = n then
        pr (index2name dummyinfo ctx x)
      else
        pr ("[bad index: " ^ (string_of_int x) ^ "/" ^ (string_of_int n)
            ^ " in {"
            ^ (List.fold_left (fun s (x,_) -> s ^ " " ^ x) "" ctx)
            ^ " }]")
  | TyInt -> pr "Int"
  | TyInductive(name, cons_list) ->
      let (ctx', name') = pickfreshname ctx name in
        pr name'; pr ". <"; 
        let pi (cons_name, ty) =
          pr cons_name; pr " "; printty_Type false ctx' ty
        in let rec pc cl = match cl with
            [] -> ()
          | [info] -> pi info
          | info::rest -> pi info; pr " | "; pc rest
        in pc cons_list; pr ">"
  | TyCompress(ty) -> pr "Compress "; printty_AType false ctx ty;
  | tyT -> pr "("; printty_Type outer ctx tyT; pr ")"

let printty ctx tyT = printty_Type true ctx tyT 

let rec printtm_Term outer ctx t = match t with
    TmIf(fi, t1, t2, t3) ->
       obox0();
       pr "if ";
       printtm_Term false ctx t1;
       print_space();
       pr "then ";
       printtm_Term false ctx t2;
       print_space();
       pr "else ";
       printtm_Term false ctx t3;
       cbox()
  | TmLet(fi, x, t1, t2) ->
       obox0();
       pr "let "; pr x; pr " = "; 
       printtm_Term false ctx t1;
       print_space(); pr "in"; print_space();
       printtm_Term false (addname ctx x) t2;
       cbox()
  | TmAbs(fi,x,tyT1,t2) ->
      (let (ctx',x') = (pickfreshname ctx x) in
         obox(); pr "lambda ";
         pr x'; pr ":"; printty_Type false ctx tyT1; pr ".";
         if (small t2) && not outer then break() else print_space();
         printtm_Term outer ctx' t2;
         cbox())
  | TmFix(fi, t1) ->
       obox();
       pr "fix "; 
       printtm_Term false ctx t1;
       cbox()
  | TmMatch(fi, t, cases) -> (
      obox(); pr "match ";
      printtm_Term outer ctx t; pr " with ";  
      let rec add_names ctx pt =
        ( match pt with
          PtUnderScore _ -> pr "_"; ctx
        | PtVar (_, name) -> 
            let (ctx', name') = pickfreshname ctx name in pr name'; ctx'
        | PtConstructor (_, name, p) -> pr name; pr " "; add_names ctx p
        | PtTuple (_, cases) -> pr "{"; let ctx' = add_cases ctx cases in pr "}"; ctx' 
        )
      and add_cases ctx cases =
        ( match cases with
          [] -> ctx
        | [p'] -> add_names ctx p'
        | p':: cases' -> let ctx' = add_names ctx p' in pr ", "; add_cases ctx' cases'
        ) in 
        List.iter (
          fun (p, t') -> pr "| "; let ctx' = add_names ctx p in pr " -> "; printtm_Term false ctx' t'
        ) cases;
      cbox()
    )
  | t -> printtm_AppTerm outer ctx t

and printtm_AppTerm outer ctx t = match t with
    TmPrimaryOp(_,(name, _, _, _),ts) ->
      pr name; 
      List.iter (fun t -> pr " "; printtm_ATerm false ctx t) ts
  | TmApp(fi, t1, t2) ->
      obox0();
      printtm_AppTerm false ctx t1;
      print_space();
      printtm_ATerm false ctx t2;
      cbox()
  | TmAlign(fi, t) -> pr "align "; printtm_ATerm false ctx t;
  | TmLabel(fi, t) -> pr "label "; printtm_ATerm false ctx t;
  | TmUnLabel(fi, t) -> pr "label "; printtm_ATerm false ctx t;
  | t -> printtm_PathTerm outer ctx t

and printtm_PathTerm outer ctx t = match t with
    TmProj(_, t1, l) ->
      printtm_ATerm false ctx t1; pr "."; pr (string_of_int l)
  | t -> printtm_ATerm outer ctx t

and printtm_ATerm outer ctx t = match t with
    TmTrue(_) -> pr "true"
  | TmFalse(_) -> pr "false"
  | TmConstructor(_, name, _, _) -> pr name
  | TmUnit(_) -> pr "unit"
  | TmVar(fi,x,n) ->
      if ctxlength ctx = n then
        pr (index2name fi ctx x)
      else
        pr ("[bad index: " ^ (string_of_int x) ^ "/" ^ (string_of_int n)
            ^ " in {"
            ^ (List.fold_left (fun s (x,_) -> s ^ " " ^ x) "" ctx)
            ^ " }]")
  | TmInt(_,s) -> pr (string_of_int s)
  | TmIndValue(_, name, t, _) -> pr name; pr " "; printtm_Term false ctx t;
  | TmTuple(fi, fields) ->
       let pf ti = printtm_Term false ctx ti 
       in let rec p l = match l with
           [] -> ()
         | [f] -> pf f
         | f::rest ->
             pf f; pr","; if outer then print_space() else break(); 
             p rest
       in pr "{"; open_hovbox 0; p fields; pr "}"; cbox()
  | t -> pr "("; printtm_Term outer ctx t; pr ")"

let printtm ctx t = printtm_Term true ctx t 

let prbinding ctx b = match b with
    NameBind -> ()
  | TmAbbBind(t,tyT) -> pr "= "; printtm ctx t
  | VarBind(tyT) -> pr ": "; printty ctx tyT
  | TyVarBind -> ()
  | TyAbbBind(tyT) -> pr "= "; printty ctx tyT 

let rec clear_compress_ty ty = match ty with
    TyId(_) -> ty
  | TyVar(_, _) -> ty
  | TyUnit -> ty
  | TyTuple(fields) ->
    let new_fields = List.map (fun field -> clear_compress_ty field) fields in 
      TyTuple(new_fields)
  | TyBool -> ty
  | TyInt -> ty
  | TyArr(source, target) -> TyArr(clear_compress_ty source, clear_compress_ty target)
  | TyInductive(name, cons_list) -> 
    let new_cons_list = List.map (fun (name, cons_type) -> (name, clear_compress_ty cons_type)) cons_list in
      TyInductive(name, new_cons_list)
  | TyCompress(ty) -> clear_compress_ty ty

let rec clear_compress_tm tm = match tm with
    TmTrue _ -> tm
  | TmFalse _ -> tm
  | TmIf(info, c, t, f) -> TmIf(info, clear_compress_tm c, clear_compress_tm t, clear_compress_tm f)
  | TmUnit _ -> tm
  | TmVar _ -> tm
  | TmInt _ -> tm
  | TmPrimaryOp(info, op, sub_list) ->
    let new_sub_list = List.map (fun sub -> clear_compress_tm sub) sub_list in 
      TmPrimaryOp (info, op, new_sub_list)
  | TmLet(info, name, def, body) ->
    TmLet(info, name, clear_compress_tm def, clear_compress_tm body)
  | TmTuple(info, contents) ->
    let new_contents = List.map (fun content -> clear_compress_tm content) contents in 
      TmTuple(info, new_contents)
  | TmProj(info, content, id) -> TmProj(info, clear_compress_tm content, id)
  | TmAbs(info, name, ty, tm) -> 
    TmAbs(info, name, clear_compress_ty ty, clear_compress_tm tm)
  | TmApp(info, func, param) -> TmApp(info, clear_compress_tm func, clear_compress_tm param)
  | TmFix(info, content) -> TmFix(info, clear_compress_tm content)
  | TmConstructor(info, name, ty1, ty2) -> TmConstructor(info, name, clear_compress_ty ty1, clear_compress_ty ty2)
  | TmIndValue(info, name, tm, ty) -> TmIndValue(info, name, clear_compress_tm tm, clear_compress_ty ty)
  | TmMatch(info, def, cases) -> 
    let new_cases = List.map (fun (pt, body) -> (pt, clear_compress_tm body)) cases in 
      TmMatch(info, clear_compress_tm def, new_cases)
  | TmAlign(info, t) -> clear_compress_tm t 
  | TmLabel(info, t) -> clear_compress_tm t 
  | TmUnLabel(info, t) -> clear_compress_tm t

let clear_compress_bind b = match b with
    NameBind -> b
  | TmAbbBind(term, ty) -> 
    let new_term = clear_compress_tm term in
    let new_ty = match ty with
      None -> None
    | Some(content) -> Some(clear_compress_ty content)
    in TmAbbBind(new_term, new_ty)
  | VarBind(ty) -> VarBind(clear_compress_ty ty)
  | TyVarBind -> TyVarBind
  | TyAbbBind(ty) -> TyAbbBind(clear_compress_ty ty)