(* Module Main: The main program.  Deals with processing the command
   line, reading files, building and connecting lexers and parsers, etc. 
   
   For most experiments with the implementation, it should not be
   necessary to change this file.
*)

open Format
open Fileutil
open Json
open Support.Pervasive
open Support.Error
open Syntax
open Core

let argDefs = [
  "-I",
      Arg.String (fun f -> searchpath := f::!searchpath),
      "Append a directory to the search path"]

let parseArgs () =
  let inFile = ref (None : string option) in
    let outFile = ref (None: string option) in
      let isAutoLabel = ref false in 
        Arg.parse argDefs
          (fun s ->
            if s = "true" then isAutoLabel := true
            else if s = "false" then isAutoLabel := false
            else
              match (!inFile, !outFile) with
                None, _ -> inFile := Some(s)
              | Some (_), None -> outFile := Some(s)
              | _ -> err "You must sepcify exactly one input file and at most one output file")
          "";
        match !inFile with
            None -> err "You must specify an input file"
          | Some(s) -> (s, !outFile, !isAutoLabel)

  
let main () = 
  let inFile, outFile, isAutoLabel = parseArgs() in
  let _ = processfile inFile isAutoLabel in
    match outFile with
      None -> ()
    | Some name -> 
      let node = file2json inFile in
        savejson node (name)

let () = set_max_boxes 1000
let () = set_margin 67
let res = 
  Printexc.catch (fun () -> 
    try main();0 
    with Exit x -> x) 
  ()
let () = print_flush()
let () = exit res
