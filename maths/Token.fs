module maths.Token

type Number =
    | Float of float
    | Integer of int

type Operator =
    | Plus
    | Minus
    | Star
    | Slash
    | Equal
    | BangEqual
    | Less
    | LessEqual
    | Greater
    | GreaterEqual
    | LeftParen
    | RightParen
    | Bang

type Lexeme =
    | Number of Number
    | String of string
    | Keyword of string
    | Operator of Operator
    | Identifier of string 

type Token = { lexeme: Lexeme; line: int }