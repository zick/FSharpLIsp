open System

let kLPar = '('
let kRPar = ')'
let kQuote = '\''

type LObj(tag : String) =
  member this.tag = tag
type Nil() =
  inherit LObj("nil")
type Num(n : Int32) =
  inherit LObj("num")
  member this.num = n
type Sym(s : String) =
  inherit LObj("sym")
  member this.str = s
type Error(s : String) =
  inherit LObj("error")
  member this.str = s
type Cons(a : LObj, d : LObj) =
  inherit LObj("cons")
  member val car = a with get, set
  member val cdr = d with get, set
type Subr(fn : LObj -> LObj) =
  inherit LObj("subr")
  member this.fn = fn
type Expr(args : LObj, body : LObj, env : LObj) =
  inherit LObj("expr")
  member this.args = args
  member this.body = body
  member this.env = env

let o obj = obj :> LObj

let kNil = o(Nil ())

let safeCar (obj : LObj) =
  match obj with
  | :? Cons as c -> c.car
  | _ -> kNil

let safeCdr (obj : LObj) =
  match obj with
  | :? Cons as c -> c.cdr
  | _ -> kNil

let makeNum n = o(Num n)
let makeError s = o(Error s)
let makeCons a d = o(Cons (a, d))
let makeSubr fn = o(Subr fn)
let makeExpr args env = o(Expr (safeCar args, safeCdr args, env))

let symTable = ref (Map.ofList [("nil", kNil)])
let makeSym str =
  if Map.containsKey str !symTable then
    Map.find str !symTable
  else
    let sym = o(Sym str) in
      symTable := Map.add str sym !symTable;
      sym

let rec nreconc (lst : LObj) (tail : LObj) =
  match lst with
  | :? Cons as c ->
      let tmp = c.cdr in
        c.cdr <- tail;
        nreconc tmp lst
  | _ -> tail
let nreverse lst = nreconc lst kNil

let pairlis (lst1 : LObj) (lst2 : LObj) =
  let rec doit (lst1 : LObj) (lst2 : LObj) acc =
    match lst1 with
    | :? Cons as cons1 ->
        match lst2 with
        | :? Cons as cons2 ->
            doit cons1.cdr cons2.cdr
                 (makeCons (makeCons cons1.car cons2.car) acc)
        | _ -> nreverse acc
    | _ -> nreverse acc
  doit lst1 lst2 kNil

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
  try makeNum (Int32.Parse str)
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
    if str1 = "" then (makeError "empty input", "")
    elif c = kRPar then (makeError ("invalid syntax: " + str), "")
    elif c = kLPar then readList rest kNil
    elif c = kQuote then readQuote rest
    else readAtom str1
and readQuote str =
  let (elm, next) = read str in
    (makeCons (makeSym "quote") (makeCons elm kNil), next)
and readList str acc =
  let (str1, c, rest) = lookAhead str in
    if str1 = "" then (makeError "unfinished parenthesis", "")
    elif c = kRPar then (nreverse acc, rest)
    else
      let (obj, next) = read str1 in
      match obj with
      | :? Error -> (obj, next)
      | _ -> readList next (makeCons obj acc)

let rec printObj (obj : LObj) =
  match obj with
  | :? Nil -> "nil"
  | :? Num as num -> num.num.ToString ()
  | :? Sym as sym -> sym.str
  | :? Error as err -> "<error: " + err.str + ">"
  | :? Cons -> "(" + (printList obj "" "") + ")"
  | :? Subr -> "<subr>"
  | :? Expr -> "<expr>"
  | _ -> "<unknown>"
and printList obj delimiter acc =
  match obj with
  | :? Cons as cons ->
      printList cons.cdr " " (acc + delimiter + (printObj cons.car))
  | :? Nil -> acc
  | _ -> acc + " . " + (printObj obj)

let rec findVarInFrame (sym : LObj) (alist : LObj) =
  match safeCar (safeCar alist) with
  | :? Sym as s -> if o(s) = sym then safeCar alist
                   else findVarInFrame sym (safeCdr alist)
  | _ -> kNil
let rec findVar (sym : LObj) (env : LObj) =
  match env with
  | :? Cons as cons ->
      match findVarInFrame sym cons.car with
      | :? Nil -> findVar sym cons.cdr
      | pair -> pair
  | _ -> kNil

let gEnv = makeCons kNil kNil

let addToEnv (sym : LObj) (value : LObj) (env : LObj) =
  match env with
  | :? Cons as cons -> cons.car <- makeCons (makeCons sym value) cons.car
  | _ -> ()

let rec eval (obj : LObj) (env : LObj) =
  match obj with
  | :? Sym ->
      match findVar obj env with
      | :? Nil -> makeError ((printObj obj) + " has no value")
      | pair -> safeCdr pair
  | :? Cons -> evalCons obj env
  | _ -> obj
and evalCons obj env =
  let opr = safeCar obj in
  let args = safeCdr obj in
    if opr = (makeSym "quote") then
      safeCar args
    elif opr = (makeSym "if") then
      let c = eval (safeCar args) env in
        match c with
        | :? Error -> c
        | :? Nil -> eval (safeCar (safeCdr (safeCdr args))) env
        | _ -> eval (safeCar (safeCdr args)) env
    elif opr = (makeSym "lambda") then
      makeExpr args env
    else apply (eval opr env) (evlis args env kNil) env
and evlis lst env acc =
  match lst with
  | :? Nil -> nreverse acc
  | _ ->
      match eval (safeCar lst) env with
      | :? Error as err -> o(err)
      | elm -> evlis (safeCdr lst) env (makeCons elm acc)
and progn (body : LObj) env acc =
  match body with
  | :? Cons as cons ->
      let v = eval cons.car env in
        match v with
        | :? Error -> v
        | _ -> progn cons.cdr env v
  | _ -> acc
and apply f args env =
  match args with
  | :? Error -> args
  | _ ->
      match f with
      | :? Error -> f
      | :? Subr as subr -> subr.fn args
      | :? Expr as expr ->
          progn expr.body (makeCons (pairlis expr.args args) expr.env) kNil
      | _ -> makeError ((printObj f) + " is not function")

let subrCar args = safeCar (safeCar args)

let subrCdr args = safeCdr (safeCar args)

let subrCons args = makeCons (safeCar args) (safeCar (safeCdr args))

let first (x, y) = x

let rec repl () =
  Console.Write "> "
  match Console.ReadLine () with
  | null -> ()
  | line ->
      Console.WriteLine (printObj (eval (first (read line)) gEnv))
      repl ()

let () =
  addToEnv (makeSym "car") (makeSubr subrCar) gEnv
  addToEnv (makeSym "cdr") (makeSubr subrCdr) gEnv
  addToEnv (makeSym "cons") (makeSubr subrCons) gEnv
  addToEnv (makeSym "t") (makeSym "t") gEnv
  repl ()
