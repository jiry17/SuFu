open Syntax
open Support.Error
open Support.Pervasive
open Yojson.Basic

val ty2json: context -> ty -> t
val tm2json: context -> term -> t
val file2json: string -> t
val printjson: t -> unit
val savejson: t -> string -> unit