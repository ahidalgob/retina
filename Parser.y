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
P : LDF program LI end ';'  { PE $1 $3 }

-- Lista de definicion de funciones
LDF : DF LDF    { LDFE $ $1 : listLDFE $2 }
    | {-empty-} { LDFE [] }

-- Definicion de funcion
DF  : func fid '(' LP ')' begin LI end ';'         { DFE $2 $4 $7 }
    | func fid '(' LP ')' '->' T begin LI end ';'  { RDFE $2 $4 $7 $9 }

-- Lista de parametros
LP : {-empty-}      { LPE [] }
   | LPNV           { LPE $ listLPNVE $1 }
   
-- Lista de Parametros no vacia (construccion auxiliar para manejar las comas)
LPNV : T id             { LPNVE [($1, $2)] }
     | T id ',' LPNV    { LPNVE $ ($1, $2) : listLPNVE $4 }

-- Tipo
T : boolean { BooleanE }
  | number  { NumberE }
  
-- Expresion (Construcciones que evaluan boolean o number
--             (o llamados a funciones que pueden evaluar void))
E   : id { IdE $1 }
    | true { TrueE }
    | false { FalseE }
    | '(' E ')' { ParE $2 }
    | E '<' E { ComparE $1 "<" $3 }
    | E '>' E { ComparE $1 ">" $3 }
    | E '<=' E { ComparE $1 "<=" $3 }
    | E '>=' E { ComparE $1 ">=" $3 }
    | E '==' E { ComparE $1 "==" $3 }
    | E '/=' E { ComparE $1 "/=" $3 }
    | not E { NotE $2 }
    | E and E { LogicE $1 "and" $3 }
    | E or E { LogicE $1 "or" $3 }
    | fid '(' LV ')' { FuncE $1 $3 }
    | '-' E         { MinusE $2 }
    | E '+' E       { AritE $1 "+" $3 }
    | E '-' E       { AritE $1 "-" $3 }
    | E '*' E       { AritE $1 "*" $3 }
    | E '/' E       { AritE $1 "/" $3 }
    | E '%' E       { AritE $1 "%" $3 }
    | E mod E       { AritE $1 "mod" $3 }
    | E div E       { AritE $1 "div" $3 }
    | n { NumberLiteralE $1 }

-- Lista de valores (argumentos de una funcion)
LV  : LVNV       { LVE $ listLVNVE $1 }
    | {-empty-}  { LVE [] }

-- Lista de valones no vacia (para manejar las comas)
LVNV    : E             { LVNVE [$1] }
        | E ',' LVNV    { LVNVE $ $1 : listLVNVE $3}


-- Lista de declaracion de variables ([(Tipo , lista de variables)])
LD  : T DST ';' LD  { LDE $ ($1, listDSTE $2) : listLDE $4}
    | ';' LD        { $2 }
    | {-empty-}     { LDE [] }

-- Lista de variables (posiblemente con un valor asignados)
DST : id '=' E          {DSTE [DeclVal $1 $3]}
    | id                {DSTE [Decl $1]}
    | id '=' E ',' DST  {DSTE $ DeclVal $1 $3 : listDSTE $5}
    | id ',' DST        {DSTE $ Decl $1 : listDSTE $3}

-- Lista de instrucciones
LI  : {-empty-}         { LIE [] }
    | I LI              { LIE $ $1 : listLIE $2}
    | ';' LI            { $2 }

-- Instruccion
I   : with LD do LI end ';'                 { WithDoE $2 $4 }
    | repeat E times LI end ';'             { RepeatE $2 $4 }
    | fid '(' LV ')' ';'                    { FuncE $1 $3 }
    | id '=' E ';'                          { AssignE $1 $3 }
    | for id from E to E do LI end ';'      { ForE $2 $4 $6 $8 }
    | for id from E to E by E do LI end ';' { ForByE $2 $4 $6 $8 $10 }
    | if E then LI end ';'                  { IfThenE $2 $4 }
    | if E then LI else LI end ';'          { IfThenElseE $2 $4 $6 }
    | while E do LI end ';'                 { WhileE $2 $4}
    | write string LPW                      { WriteE $ PWSE $2 : listLPWE $3 }
    | write E LPW                           { WriteE $ PWEE $2 : listLPWE $3 }
    | writeln string LPW                    { WritelnE $ PWSE $2 : listLPWE $3 }
    | writeln E LPW                         { WritelnE $ PWEE $2 : listLPWE $3 }
    | read id ';'                           { ReadE $2 }
    | return E ';'                          { ReturnE $2 }
    
-- Lista de Imprimibles (expresiones o strings)
LPW : ';'                               { LPWE [] }
    | ',' string LPW                    { LPWE $ PWSE $2 : listLPWE $3 }
    | ',' E LPW                         { LPWE $ PWEE $2 : listLPWE $3 }

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
