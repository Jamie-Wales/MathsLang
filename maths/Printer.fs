module maths.Printer

open Grammar
open Token

let rec printAST expr =
    printASTWithIndent expr 0

and printASTWithIndent expr indent =
    let indentStr = String.replicate indent "  "
    match expr with
    | Literal lit -> printLiteral lit indentStr
    | Unary (op, expr) -> printUnary op expr indentStr indent
    | Binary (left, op, right) -> printBinary left op right indentStr indent
    | Grouping expr -> printGrouping expr indentStr indent

and printLiteral (lit: Literal) indentStr =
    match lit with
    | Literal.Number n -> printfn "%s%s" indentStr (numberToString n)
    | Literal.String s -> printfn "%sString(\"%s\")" indentStr s
    | Literal.Bool b -> printfn "%sBool(%b)" indentStr b
    | Literal.Nil -> printfn "%sNil" indentStr

and printUnary op expr indentStr indent =
    printfn $"%s{indentStr} Unary"
    printfn $"%s{indentStr} Operator: %s{tokenToString op}"
    printASTWithIndent expr (indent + 1)

and printBinary left op right indentStr indent =
    printfn $"%s{indentStr} Binary"
    printfn $"%s{indentStr} Left:"
    printASTWithIndent left (indent + 2)
    printfn $"%s{indentStr}  Operator: %s{tokenToString op}"
    printfn $"%s{indentStr}  Right:"
    printASTWithIndent right (indent + 2)

and printGrouping expr indentStr indent =
    printfn $"%s{indentStr}Grouping"
    printASTWithIndent expr (indent + 1)

and numberToString (n: Number) =
    match n with
    | Float f -> $"Number(Float %f{f})"
    | Integer i -> $"Number(Integer %d{i})"

and tokenToString (token: Token) =
    match token.lexeme with
    | Lexeme.Number n -> numberToString n
    | Lexeme.String s -> $"String(\"%s{s}\")"
    | Lexeme.Keyword k -> $"Keyword(%s{k})"
    | Lexeme.Operator op -> operatorToString op
    | Lexeme.Identifier i -> $"Identifier(%s{i})"

and operatorToString (op: Operator) =
    match op with
    | Plus -> "+"
    | Minus -> "-"
    | Star -> "*"
    | Slash -> "/"
    | Equal -> "=="
    | BangEqual -> "!="
    | Less -> "<"
    | LessEqual -> "<="
    | Greater -> ">"
    | GreaterEqual -> ">="
    | LeftParen -> "("
    | RightParen -> ")"
    | Bang -> "!"

let printStmt stmt indent =
    let indentStr = String.replicate indent "  "
    match stmt with
    | Expression expr ->
        printfn $"%s{indentStr}Expression Statement:"
        printASTWithIndent expr (indent + 1)
    | Print expr ->
        printfn $"%s{indentStr}Print Statement:"
        printASTWithIndent expr (indent + 1)

let printProgram (program: Program) =
    printfn "Program:"
    List.iteri (fun i stmt -> 
        printfn $"Statement %d{i + 1}:"
        printStmt stmt 1
    ) program