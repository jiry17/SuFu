open Syntax

val searchpath: (string list) ref
val parsefile: string -> context -> ((string list * command) list * context)
val processfile: string -> bool -> unit 