open System

let kLPar = '('
let kRPar = ')'
let kQuote = '\''

type Obj =
  | Nil
  | Num of Int32
  | Sym of String
  | Error of String
  | Cons of (Obj ref) * (Obj ref)
  | Subr of (Obj -> Obj)
  | Expr of Obj * Obj * Obj

let safeCar obj =
  match obj with
  | Cons(a, d) -> !a
  | _ -> Nil

let safeCdr obj =
  match obj with
  | Cons(a, d) -> !d
  | _ -> Nil

let symTable = ref (Map.ofList [("nil", Nil)])
let makeSym str =
  if Map.containsKey str !symTable then
    Map.find str !symTable
  else
    let sym = Sym str in
      symTable := Map.add str sym !symTable;
      sym

let isSpace c =
  c = '\t' || c = '\r' || c = '\n' || c = ' '

let isDelimiter c =
  c = kLPar || c = kRPar || c = kQuote || isSpace c

let skipSpaces str =
  let rec doit i =
    if i = String.length str then ""
    elif isSpace str.[i] then doit (i + 1)
    else str.Substring(i)
  doit 0

let makeNumOrSym str =
  try Num (Int32.Parse str)
  with
   | :? FormatException -> makeSym str

let position f str =
  let rec doit i =
    if i = String.length str then None
    elif f str.[i] then Some i
    else doit (i + 1)
  doit 0

let readAtom str =
  match position isDelimiter str with
  | Some n -> (makeNumOrSym (str.Substring (0, n)), str.Substring n)
  | None -> (makeNumOrSym str, "")

let read str =
  let str1 = skipSpaces str in
  let c = if str1 = "" then '_' else str.[0] in
    if str1 = "" then (Error "empty input", "")
    elif c = kRPar then (Error ("invalid syntax: " + str), "")
    elif c = kLPar then (Error "noimpl", "")
    elif c = kQuote then (Error "noimpl", "")
    else readAtom str1

let printObj obj =
  match obj with
  | Nil -> "nil"
  | Num num -> num.ToString ()
  | Sym name -> name
  | Error msg -> "<error: " + msg + ">"
  | Cons _ -> "CONS"
  | Subr _ -> "<subr>"
  | Expr _ -> "<expr>"

let first (x, y) = x

let rec repl () =
  Console.Write "> "
  match Console.ReadLine () with
  | null -> ()
  | line ->
      Console.WriteLine (printObj (first (read line)))
      repl ()

let () =
  repl ()
