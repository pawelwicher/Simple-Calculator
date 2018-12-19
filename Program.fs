open System
open FParsec

let ws = spaces

let str_ws s = pstring s >>. ws

let number = pfloat .>> ws

let opp = new OperatorPrecedenceParser<float,unit,unit>()
let expr = opp.ExpressionParser
opp.TermParser <- number <|> between (str_ws "(") (str_ws ")") expr

opp.AddOperator(InfixOperator("+", ws, 1, Associativity.Left, (+)))
opp.AddOperator(InfixOperator("-", ws, 1, Associativity.Left, (-)))
opp.AddOperator(InfixOperator("*", ws, 2, Associativity.Left, (*)))
opp.AddOperator(InfixOperator("/", ws, 2, Associativity.Left, (/)))
opp.AddOperator(InfixOperator("^", ws, 3, Associativity.Right, fun x y -> System.Math.Pow(x, y)))
opp.AddOperator(PrefixOperator("-", ws, 4, true, fun x -> -x))

let ws1 = nextCharSatisfiesNot isLetter >>. ws
opp.AddOperator(PrefixOperator("sqrt", ws1, 4, true, Math.Sqrt))
opp.AddOperator(PrefixOperator("log", ws1, 4, true, Math.Log))
opp.AddOperator(PrefixOperator("exp", ws1, 4, true, Math.Exp))

let completeExpression = ws >>. expr .>> eof

let calculate expression = 
    match run completeExpression expression with
    | Success (v, _, _) -> printfn "%f" v
    | Failure (msg, _, _) -> printf "%s" msg

[<EntryPoint>]
let main _ =
    calculate "(1+2)*5"
    calculate "sqrt 2"
    printfn "Press any key..."
    Console.ReadKey() |> ignore
    0