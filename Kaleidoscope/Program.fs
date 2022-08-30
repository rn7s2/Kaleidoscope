open Kaleidoscope
open Lexer

[<EntryPoint>]
let main args =
    let lexer = Lexer()
    printfn $"{lexer.GetNextToken()}"

    0
