(* JsonSupport *)
open Support.Error
open Support.Pervasive
open Syntax
open Fileutil
open Yojson.Basic

let buildnode ty cs = `Assoc(("type", `String(ty))::cs)

let rec ty2json ctx ty = match ty with
    TyVar(x, n) ->
      if n <> (ctxlength ctx) then error dummyinfo ("expected "^(string_of_int x)^"/"^(string_of_int n)^", but got"^(ctx2string ctx)^".")
      else buildnode "var" [("name", `String (index2name dummyinfo ctx x))]
  | TyId name -> buildnode "var" [("name", `String name)]
  | TyUnit -> buildnode "unit" []
  | TyTuple cs ->
      let cs' = List.map (fun ty -> ty2json ctx ty) cs in
        buildnode "tuple" [("fields", `List cs')]
  | TyBool -> buildnode "bool" []
  | TyInt -> buildnode "int" []
  | TyArr (s, t) -> buildnode "arrow" [("s", ty2json ctx s); ("t", ty2json ctx t)]
  | TyInductive (name, cs) ->
      let ctx' = addbinding ctx name NameBind in
        let cs' = List.map 
          (fun (cname, cty) -> 
            `Assoc([("name", `String cname); ("subtype", ty2json ctx' cty)])
          ) cs in
          let cjson = ("constructors", `List cs') in
            buildnode "inductive" [("name", `String name); cjson]
  | TyCompress ty -> 
      let c = ("content", ty2json ctx ty) in
        buildnode "compress" [c]

let rec pt2json pt = match pt with
    PtUnderScore _ -> buildnode "underscore" []
  | PtVar (_, name) -> buildnode "var" [("name", `String name)]
  | PtTuple (_, cs) ->
    let cs' = List.map (fun cpt -> pt2json cpt) cs in
      let cjson = `List cs' in
        buildnode "tuple" [("fields", cjson)]
  | PtConstructor (_, name, spt) ->
    buildnode "constructor" [("name", `String name); ("content", pt2json spt)]

let rec addptbinding ctx pt = match pt with
    PtUnderScore _ -> ctx
  | PtVar (_, name) -> addbinding ctx name NameBind
  | PtTuple (_, cs) ->
    List.fold_left (fun pctx -> fun spt -> addptbinding pctx spt) ctx cs
  | PtConstructor (_, _, cpt) -> addptbinding ctx cpt
          
let rec tm2json ctx tm = match tm with
    TmTrue _ -> buildnode "true" []
  | TmFalse _ -> buildnode "false" [] 
  | TmIf (_, c, t, f) ->
    buildnode "if" [("condition", tm2json ctx c); ("true", tm2json ctx t); ("false", tm2json ctx f)]
  | TmUnit _ -> buildnode "unit" []
  | TmVar (fi, x, n) -> 
    if n <> (ctxlength ctx) then error dummyinfo ("expected "^(string_of_int x)^"/"^(string_of_int n)^", but got"^(ctx2string ctx)^".")
    else buildnode "var" [("name", `String (index2name fi ctx x))]
  | TmInt (_, w) -> buildnode "int" [("value", `Int w)]
  | TmPrimaryOp (_, (name, _, _, _), ts) -> 
      buildnode "op" [("operator", `String name); ("operand", `List (List.map (fun t -> tm2json ctx t) ts))]
  | TmLet (_, name, t1, t2) ->
    let t1' = tm2json ctx t1 in
      let ctx' = addbinding ctx name NameBind in
        let t2' = tm2json ctx' t2 in
          buildnode "let" [("name", `String name); ("def", t1'); ("content", t2')]
  | TmTuple (_, fields) ->
    let fields' = List.map (fun t -> tm2json ctx t) fields in
      buildnode "tuple" [("fields", `List fields')]
  | TmProj (_, t, v) ->
    let t' = tm2json ctx t in
      buildnode "proj" [("content", t'); ("index", `Int v)]
  | TmAbs (_, name, ty, t) ->
      let ctx' = addbinding ctx name NameBind in
        let ty' = ty2json ctx ty in
          let t' = tm2json ctx' t in
            buildnode "abs" [("name", `String name); ("vartype", ty'); ("content", t')]
  | TmApp (_, func, param) -> 
      buildnode "app" [("func", tm2json ctx func); ("param", tm2json ctx param)]
  | TmFix (_, t) -> buildnode "fix" [("content", tm2json ctx t)]
  | TmConstructor (_, name, _, _) -> buildnode "constructor" [("name", `String name)]
  | TmIndValue (_, name, t, _) -> 
      buildnode "inductive" [("name", `String name); ("w", tm2json ctx t)]
  | TmMatch (_, t, cases) ->
      let t' = tm2json ctx t in
        let case2json (pt, branch) = (
          let ctx' = addptbinding ctx pt in 
            let branch' = tm2json ctx' branch in
              let pt' = pt2json pt in
                `Assoc [("pattern", pt'); ("branch", branch')]
        ) in let cases' = List.map (fun case -> case2json case) cases in
          buildnode "match" [("value", t'); ("cases", `List cases')]
  | TmLabel (_, t) -> buildnode "label" [("content", tm2json ctx t)]
  | TmUnLabel (_, t) -> buildnode "unlabel" [("content", tm2json ctx t)]
  | TmAlign (_, t) -> buildnode "align" [("content", tm2json ctx t)]

let rec _file2json ctx f imported = 
  if List.mem f (!imported) then 
    (ctx,  `List []) 
  else
    (imported := f::!imported;
    let cmds, _ = parsefile f ctx in
      let g (pctx, pnodes) (decorates, c) = 
        let d_node = List.map (fun d -> `String d) decorates in 
          (let (pctx', node) = _command2json pctx c imported in
            (pctx', (`Assoc [("node", node); ("decorates", `List d_node)])::pnodes)) in 
        let (ctx', nodes) = List.fold_left g (ctx, []) cmds in
          (ctx', `List (List.rev nodes)))
and _cvalue2json v = match v with
    IntConfig i -> buildnode "int" [("value", `Int i)]
  | BoolConfig v -> buildnode "bool" [("value", `Bool v)]
  | StringConfig s -> buildnode "string" [("value", `String s)]
and _command2json ctx c imported = match c with
    Import name -> 
      let (ctx', node) = _file2json ctx name imported in
        (ctx', buildnode "import" [("name", `String name); ("content", node)])
  | Config (name, value) ->
      (ctx, buildnode "config" [("name", `String name); ("value", _cvalue2json value)])
  | Eval (_, t) -> (ctx, buildnode "skip" [])
  | Bind (_, name, b) ->
      let node = _bind2json ctx b in
        let ctx' = addbinding ctx name NameBind in
          (ctx', buildnode "bind" [("name", `String name); ("def", node)])
  | DefInductive (_, name, cs) ->
      let ty = TyInductive (name, cs) in
        let tynode = ty2json ctx ty in
          let ctx1 = addbinding ctx name NameBind in
            let ctx2 = List.fold_left (fun pctx -> fun (name, _) -> addbinding pctx name NameBind) ctx1 cs in
              (ctx2, buildnode "defind" [("indtype", tynode)])
and _bind2json ctx b = match b with
    TmAbbBind (t, _) -> buildnode "term" [("content", tm2json ctx t)]
  | TyAbbBind (ty) -> buildnode "type" [("content", ty2json ctx ty)]
  | VarBind (ty) -> buildnode "var" [("content", ty2json ctx ty)]
  | _ -> buildnode "skip" [] 

let file2json name =
  let imported = ref ([]: string list) in
    let (_, node) = _file2json emptycontext name imported in
      node

let printjson json =
  let s = pretty_to_string json in
    pr s

let savejson json path = 
    to_file path json