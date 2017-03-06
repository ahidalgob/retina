-- Retina - Proyecto de Traductores
-- Lexer
-- Augusto Hidalgo 13-10665
-- Genesis Kufatty 13-10708

{
module Parser where
import Lexer
import AST
}

%name parse
%tokentype { Token }

%token
    while           { WhileTK _ }
    for             { ForTK _ }
    from            { FromTK _ }
    to              { ToTK _ }
    by              { ByTK _ }
    begin           { BeginTK _ }
    func            { FuncTK _ }
    return          { ReturnTK _ }
    repeat          { RepeatTK _ }
    program         { ProgramTK _ }
    with            { WithTK _ }
    do              { DoTK _ }
    end             { EndTK _ }
    times           { TimesTK _ }
    not             { NotTK _ }
    and             { AndTK _ }
    or              { OrTK _ }
    read            { ReadTK _ }
    write           { WriteTK _ }
    writeln         { WritelnTK _ }
    if              { IfTK _ }
    then            { ThenTK _ }
    else            { ElseTK _ }
    number          { NumberTK _ }
    boolean         { BooleanTK _ }
    true            { TrueTK _ }
    false           { FalseTK _ }
    div             { DivTK _ }
    mod             { ModTK _ }
    ','             { CommaTK _ }
    n               { NumLiteralTK _ $$ }
    string          { StringTK _ $$ }
    id              { IdTK _ $$ }
    fid             { FuncIdTK _ $$ }
    '+'             { PlusTK _ }
    '=='            { EqualTK _ }
    '*'             { ProductTK _ }
    '-'             { MinusTK _ }
    '%'             { RestTK _ }
    '/'             { DivExacTK _ }
    '/='            { DifTK _ }
    '>='            { GreaterEqualTK _ }
    '<='            { LessEqualTK _ }
    '>'             { GreaterTK _ }
    '<'             { LessTK _ }
    '='             { AssignTK _ }
    '('             { ParenOpenTK _ }
    ')'             { ParenCloseTK _ }
    ';'             { SemicolonTK _ }
    '->'            { TypeTK _ }


%left or
%left and
%nonassoc '>' '<' '==' '/=' '>=' '<=' 
%left '+' '-'
%left '*' '/' '%' mod div
%nonassoc not

%%
-- Program
P : LDF program LI end ';'  { PN $1 $3 }

-- Lista de definicion de funciones
LDF : DF LDF    { LDFN $ $1 : listLDFN $2 }
    | {-empty-} { LDFN [] }

-- Definicion de funcion
DF  : func fid '(' LP ')' begin LI end ';'         { DFN $2 $4 $7 }
    | func fid '(' LP ')' '->' T begin LI end ';'  { RDFN $2 $4 $7 $9 }

-- Lista de parametros
LP : {-empty-}      { LPN [] }
   | LPNV           { LPN $ listLPNVN $1 }
   
-- Lista de Parametros no vacia (construccion auxiliar para manejar las comas)
LPNV : T id             { LPNVN [($1, $2)] }
     | T id ',' LPNV    { LPNVN $ ($1, $2) : listLPNVN $4 }

-- Tipo
T : boolean { BooleanN }
  | number  { NumberN }
  
-- Expresion (Construcciones que evaluan boolean o number
--             (o llamados a funciones que pueden evaluar void))
E   : id { IdN $1 }
    | true { TrueN }
    | false { FalseN }
    | '(' E ')' { ParN $2 }
    | E '<' E { ComparN $1 "<" $3 }
    | E '>' E { ComparN $1 ">" $3 }
    | E '<=' E { ComparN $1 "<=" $3 }
    | E '>=' E { ComparN $1 ">=" $3 }
    | E '==' E { ComparN $1 "==" $3 }
    | E '/=' E { ComparN $1 "/=" $3 }
    | not E { NotN $2 }
    | E and E { LogicN $1 "and" $3 }
    | E or E { LogicN $1 "or" $3 }
    | fid '(' LV ')' { FuncN $1 $3 }
    | '-' E         { MinusN $2 }
    | E '+' E       { AritN $1 "+" $3 }
    | E '-' E       { AritN $1 "-" $3 }
    | E '*' E       { AritN $1 "*" $3 }
    | E '/' E       { AritN $1 "/" $3 }
    | E '%' E       { AritN $1 "%" $3 }
    | E mod E       { AritN $1 "mod" $3 }
    | E div E       { AritN $1 "div" $3 }
    | n { NumberLiteralN $1 }

-- Lista de valores (argumentos de una funcion)
LV  : LVNV       { LVN $ listLVNVN $1 }
    | {-empty-}  { LVN [] }

-- Lista de valones no vacia (para manejar las comas)
LVNV    : E             { LVNVN [$1] }
        | E ',' LVNV    { LVNVN $ $1 : listLVNVN $3}


-- Lista de declaracion de variables ([(Tipo , lista de variables)])
LD  : T DST ';' LD  { LDN $ ($1, listDSTN $2) : listLDN $4}
    | ';' LD        { $2 }
    | {-empty-}     { LDN [] }

-- Lista de variables (posiblemente con un valor asignados)
DST : id '=' E          {DSTN [DeclVal $1 $3]}
    | id                {DSTN [Decl $1]}
    | id '=' E ',' DST  {DSTN $ DeclVal $1 $3 : listDSTN $5}
    | id ',' DST        {DSTN $ Decl $1 : listDSTN $3}

-- Lista de instrucciones
LI  : {-empty-}         { LIN [] }
    | I LI              { LIN $ $1 : listLIN $2}
    | ';' LI            { $2 }

-- Instruccion
I   : with LD do LI end ';'                 { WithDoN $2 $4 }
    | repeat E times LI end ';'             { RepeatN $2 $4 }
    | id '=' E ';'                          { AssignN $1 $3 }
    | for id from E to E do LI end ';'      { ForN $2 $4 $6 $8 }
    | for id from E to E by E do LI end ';' { ForByN $2 $4 $6 $8 $10 }
    | if E then LI end ';'                  { IfThenN $2 $4 }
    | if E then LI else LI end ';'          { IfThenElseN $2 $4 $6 }
    | while E do LI end ';'                 { WhileN $2 $4}
    | write string LPW                      { WriteN $ PWSN $2 : listLPWN $3 }
    | write E LPW                           { WriteN $ PWEN $2 : listLPWN $3 }
    | writeln string LPW                    { WritelnN $ PWSN $2 : listLPWN $3 }
    | writeln E LPW                         { WritelnN $ PWEN $2 : listLPWN $3 }
    | read id ';'                           { ReadN $2 }
    | return E ';'                          { ReturnN $2 }
    | E ';'                                 { ExprN $1 }
    
-- Lista de Imprimibles (expresiones o strings)
LPW : ';'                               { LPWN [] }
    | ',' string LPW                    { LPWN $ PWSN $2 : listLPWN $3 }
    | ',' E LPW                         { LPWN $ PWEN $2 : listLPWN $3 }

{

happyError :: [Token] -> a
happyError tks = error ("Error en el parser en " ++ lcn ++ "\n" ++ "happy is sad :(")
    where
        lcn =   case tks of
                    [] -> "el final del archivo"
                    tk:_ -> "linea " ++ show l ++ ", columna " ++ show c ++ "\n" ++ show tk
                        where
                            AlexPn _ l c = tokenPosn tk
}
