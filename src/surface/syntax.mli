(* module Syntax: syntax trees and associated support functions *)

open Support.Pervasive
open Support.Error

(* Data type definitions *)

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

(* Contexts *)
type context
val emptycontext : context 
val ctx2string : context -> string
val ctxlength : context -> int
val addbinding : context -> string -> binding -> context
val addpattern : context -> pattern -> context
val addname: context -> string -> context
val index2name : info -> context -> int -> string
val getbinding : info -> context -> int -> binding
val name2index : info -> context -> string -> int
val isnamebound : context -> string -> bool
val getTypeFromContext : info -> context -> int -> ty
val ptsize : pattern -> int
val getproduct : ty -> (ty list) option
val getconstructty : ty -> string -> ty option

(* Shifting and substitution *)
val termShift: int -> term -> term
val termSubstTop: term -> term -> term
val typeShift : int -> ty -> ty
val typeSubstTop: ty -> ty -> ty
val tytermSubstTop: ty -> term -> term

(* Printing *)
val printtm: context -> term -> unit
val printtm_ATerm: bool -> context -> term -> unit
val printty : context -> ty -> unit
val prbinding : context -> binding -> unit

(* Misc *)
val tmInfo: term -> info

(* Compress Label Clear*)
val clear_compress_tm: term -> term
val clear_compress_ty: ty -> ty
val clear_compress_bind: binding -> binding