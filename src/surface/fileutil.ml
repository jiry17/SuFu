open Syntax
open Core
open Format
open Support.Error
open Support.Pervasive

let searchpath = ref [""]

let openfile infile = 
  let rec trynext l = match l with
        [] -> err ("Could not find " ^ infile)
      | (d::rest) -> 
          let name = if d = "" then infile else (d ^ "/" ^ infile) in
          try open_in name
            with Sys_error m -> trynext rest
  in trynext !searchpath

let parsefile inFile =
  let pi = openfile inFile
  in pr "\n"; let lexbuf = Lexer.create inFile pi
  in let result =
    try Parser.toplevel Lexer.main lexbuf with Parsing.Parse_error -> 
    error (Lexer.info lexbuf) "Parse error"
in
  Parsing.clear_parser(); close_in pi; result

let checkbinding fi ctx b = match b with
  NameBind -> NameBind
| VarBind(tyT) -> VarBind(tyT)
| TmAbbBind(t,None) -> TmAbbBind(t, Some(typeof ctx t))
| TmAbbBind(t,Some(tyT)) ->
   let tyT' = typeof ctx t in
   if tyeqv ctx tyT' tyT then TmAbbBind(t,Some(tyT))
   else error fi "Type of binding does not match declared type"
| TyVarBind -> TyVarBind
| TyAbbBind(tyT) -> TyAbbBind(tyT)

let prbindingty ctx b = match b with
  NameBind -> ()
| VarBind(tyT) -> pr ": "; printty ctx tyT 
| TmAbbBind(t, tyT_opt) -> pr ": ";
   (match tyT_opt with
       None -> printty ctx (typeof ctx t)
     | Some(tyT) -> printty ctx tyT)
| TyVarBind -> ()
| TyAbbBind(tyT) -> pr ":: "; printty ctx tyT

let rec _processfile f ctx imported is_auto =
  if List.mem f (!imported) then
    ctx
  else (
    imported := f :: !imported;
    let cmds,_ = parsefile f ctx in 
    let g ctx (decros, c) =  
      open_hvbox 0;
      let results = _processcommand ctx c imported is_auto in
      print_flush();
      results
    in
      List.fold_left g ctx cmds)

  and _processcommand ctx cmd imported is_auto = match cmd with
    Import(f) -> 
      _processfile f ctx imported is_auto
  | Config(name, v) -> ctx
  | Eval(fi,pre_t) -> 
      let t = if is_auto then clear_compress_tm pre_t else pre_t in
      let tyT = typeof ctx t in
      let t' = eval ctx t in
      printtm_ATerm true ctx t'; 
      print_break 1 2;
      pr ": ";
      printty ctx tyT;
      force_newline();
      ctx
  | Bind(fi,x,pre_bind) -> 
      let bind = if is_auto then clear_compress_bind pre_bind else pre_bind in
      let bind = checkbinding fi ctx bind in
      let bind' = evalbinding ctx bind in
      pr x; pr " "; prbindingty ctx bind'; force_newline();
      addbinding ctx x bind'
  | DefInductive(fi, x, pre_cons_list) ->
      let cons_list = if is_auto then 
        List.map (fun (name, cons_ty) -> (name, clear_compress_ty cons_ty)) pre_cons_list
      else pre_cons_list in 
      let ty = TyInductive(x, cons_list) in
        let ctx' = _processcommand ctx (Bind(fi, x, TyAbbBind(ty))) imported is_auto in
          let (_, ctx'') = (List.fold_left 
            (fun (pc, pctx) -> fun (name, cons_ty) -> 
              let cons_ty' = typeShift pc cons_ty in
                let ty' = typeShift pc ty in
                  let c' = (Bind (fi, name, TmAbbBind(TmConstructor(fi, name, ty', (typeSubstTop ty' cons_ty')), None))) in 
                  (pc + 1, _processcommand pctx c' imported is_auto)
            ) (1, ctx') cons_list)
            in ctx''

let processfile f is_auto = 
  let imported = ref ([]: string list) in
    let _ = _processfile f emptycontext imported is_auto in 
      ()