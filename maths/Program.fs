module main 

open System
open System.IO
open maths
open Token


let parseAndPrintExpression (tokens: Token list) =
    try
        let expr = Parser.parseTokens tokens
        printfn "Parsed Expression:"
        Printer.printAST expr
        printfn ""
    with
    | ex -> printfn "Error parsing expression: %s" ex.Message

let rec repl() =
    printf "> "
    let input = Console.ReadLine()
    match input with
    | null | "exit" | "quit" -> ()
    | _ ->
        let tokens = Scanner.tokenize input
        parseAndPrintExpression tokens
        repl()

let processFile (path: string) =
    try
        let content = File.ReadAllText(path)
        printfn "Processing file: %s" path
        printfn "Content:"
        printfn "%s" content
        printfn "\nTokens:"
        let tokens = Scanner.tokenize content
        tokens |> List.iter (printfn "%A")
        printfn "\nParsed Expressions:"
        
        let rec parseExpressions remainingTokens =
            if List.isEmpty remainingTokens then
                ()
            else
                try
                    let expr = Parser.parseTokens remainingTokens
                    Printer.printAST expr
                    printfn ""
                    let consumedTokens = Parser.createParserState(remainingTokens).Position
                    parseExpressions (List.skip consumedTokens remainingTokens)
                with
                | ex -> 
                    printfn $"Error parsing expression: %s{ex.Message}"
                    parseExpressions (List.tail remainingTokens)  
        
        parseExpressions tokens
    with
    | ex -> printfn "Error processing file: %s" ex.Message

[<EntryPoint>]
let main argv =
    match argv with
    | [|file|] when File.Exists(file) -> processFile file
    | _ ->
        printfn "Maths Language REPL"
        printfn "Type 'exit' or 'quit' to end the session"
        repl()
    0