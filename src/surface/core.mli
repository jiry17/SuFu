(* module Core

   Core typechecking and evaluation functions
*)

open Syntax
open Support.Error

val eval : context -> term -> term 
val typeof : context -> term -> ty
val tyeqv : context -> ty -> ty -> bool
val evalbinding : context -> binding -> binding 
val simplifyty : context -> ty -> ty

(* JsonSupport 
val tm2json: context -> term -> unit
val ty2json: context -> term -> unit
val binding2json: context -> term -> unit 
*)
