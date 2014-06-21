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

let makeCons a d = Cons(ref a, ref d)

let rec nreconc lst tail =
  match lst with
  | Cons(a, d) ->
      let tmp = !d in
        d := tail;
        nreconc tmp lst
  | _ -> tail
let nreverse lst = nreconc lst Nil

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

let lookAhead str =
  let str1 = skipSpaces str in
  let c = if str1 = "" then '_' else str.[0] in
  let rest = if str1 = "" then ""
             else str.Substring 1 in
    (str1, c, rest)


let rec read str =
  let (str1, c, rest) = lookAhead str in
    if str1 = "" then (Error "empty input", "")
    elif c = kRPar then (Error ("invalid syntax: " + str), "")
    elif c = kLPar then readList rest Nil
    elif c = kQuote then readQuote rest
    else readAtom str1
and readQuote str =
  let (elm, next) = read str in
    (makeCons (makeSym "quote") (makeCons elm Nil), next)
and readList str acc =
  let (str1, c, rest) = lookAhead str in
    if str1 = "" then (Error "unfinished parenthesis", "")
    elif c = kRPar then (nreverse acc, rest)
    else
      match read str1 with
      | (Error e, next) -> (Error e, next)
      | (elm, next) -> readList next (makeCons elm acc)

let rec printObj obj =
  match obj with
  | Nil -> "nil"
  | Num num -> num.ToString ()
  | Sym name -> name
  | Error msg -> "<error: " + msg + ">"
  | Cons _ -> "(" + (printList obj "" "") + ")"
  | Subr _ -> "<subr>"
  | Expr _ -> "<expr>"
and printList obj delimiter acc =
  match obj with
  | Cons(a, d) -> printList (!d) " " (acc + delimiter + (printObj !a))
  | Nil -> acc
  | _ -> acc + " . " + (printObj obj)

let rec findVarInFrame str alist =
  match safeCar (safeCar alist) with
  | Sym k -> if k = str then safeCar alist
             else findVarInFrame str (safeCdr alist)
  | _ -> Nil
let rec findVar sym env =
  match (env, sym) with
  | (Cons(a, d), Sym str) ->
      match findVarInFrame str !a with
      | Nil -> findVar sym !d
      | pair -> pair
  | _ -> Nil

let gEnv = makeCons Nil Nil

let addToEnv sym value env =
  match env with
  | Cons(a, d) -> a := makeCons (makeCons sym value) !a
  | _ -> ()

let rec eval obj env =
  match obj with
  | Sym _ ->
      match findVar obj env with
      | Nil -> Error ((printObj obj) + " has no value")
      | pair -> safeCdr pair
  | Cons _ -> evalCons obj env
  | _ -> obj
and evalCons obj env =
  Error "noimpl"

let first (x, y) = x

let rec repl () =
  Console.Write "> "
  match Console.ReadLine () with
  | null -> ()
  | line ->
      Console.WriteLine (printObj (eval (first (read line)) gEnv))
      repl ()

let () =
  addToEnv (makeSym "t") (makeSym "t") gEnv
  repl ()
