// Learn more about F# at http://fsharp.org

open System
open Ast
open Randomize
open Simplify
open Differential
open Latex

let limit = ref 1000

[<EntryPoint>]
let main argv =

    let e = Randomize.expr 3 |> Simplify.iter !limit 

    printfn "---- ---- ---- Source ---- ---- ----"
    printfn "%s" (Latex.eval e)

    printfn "---- ---- ----  Diff  ---- ---- ----"
    let r = Differential.cul e |> Simplify.iter !limit
    printfn "%s" (Latex.eval r)
  
    printfn ""

    0