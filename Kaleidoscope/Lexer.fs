namespace Kaleidoscope

open System
open System.Text

module Lexer =
    type Token =
        | Eof
        | Def
        | Extern
        | Identifier
        | Number

    type Lexer() =
        let mutable identifier = String.Empty
        let mutable number = 0.0
        let EOF = -1
        let reader = stdin
        let identifierBuilder = new StringBuilder()
        let numberBuilder = new StringBuilder()

        member this.GetLastIdentifier() = identifier
        member this.GetLastNumber() = number

        member this.GetNextToken() =
            let mutable c = (int ' ')

            while Char.IsWhiteSpace((char c)) do
                c <- reader.Read()

            // identifier: [a-zA-Z][a-zA-Z0-9]*
            if Char.IsLetter((char c)) then
                identifierBuilder.Append((char c)) |> ignore
                c <- reader.Read()

                while Char.IsLetterOrDigit((char c)) do
                    identifierBuilder.Append((char c)) |> ignore
                    c <- reader.Read()

                identifier <- identifierBuilder.ToString()
                identifierBuilder.Clear() |> ignore

                match identifier with
                | "def" -> Token.Def
                | "extern" -> Token.Extern
                | _ -> Token.Identifier

            // number: [0-9.]+
            elif Char.IsDigit((char c)) || (char c) = '.' then
                numberBuilder.Append((char c)) |> ignore
                c <- reader.Read()

                while Char.IsDigit((char c)) || (char c) = '.' do
                    numberBuilder.Append((char c)) |> ignore
                    c <- reader.Read()

                number <- Double.Parse(numberBuilder.ToString())
                numberBuilder.Clear() |> ignore
                Token.Number

            // comments
            elif (char c) = '#' then
                c <- reader.Read()

                while c <> EOF && (char c) <> '\n' && (char c) <> '\r' do
                    c <- reader.Read()

                if c <> EOF then
                    this.GetNextToken()
                else
                    Token.Eof

            // end of file. Don't eat the EOF.
            elif c = EOF then
                Token.Eof
            else
                failwith $"Unknown token {(char c)}!"
