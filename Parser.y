{
module Parser where
import Lexer
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
    ';'             { Semicolon _ }
    '->'            { TypeTK _ }

%nonassoc not
%left and
%left or
%nonassoc '>' '<' '==' '/=' '>=' '<=' 
%left '+' '-'
%left '*' '/'
%left '%' mod div

%%

P : LDF program LBLOCK end ';'  { PE $1 $3 }

LBLOCK  : {-empty-}                     { LBLOCKE [] }
        | with LD do LI end ';' LBLOCK  { LBLOCKE $ WithDoE $2 $4 : listLBLOCKE $7 }

LDF : DF LDF    { LDFE $ $1 : listLDFE $2 }
    | {-empty-} { LDFE [] }

DF  : func fid '(' LP ')' begin LIR end ';'         { DFE $2 $4 $7 }
    | func fid '(' LP ')' '->' T begin LIR end ';'  { RDFE $2 $4 $7 $9 }

LP : {-empty-}      { LPE [] }
   | LPNV           { LPE $ listLPNVE $1 }
   
LPNV : T id             { LPNVE [($1, $2)] }
     | T id ',' LPNV    { LPNVE $ ($1, $2) : listLPNVE $4 }

T : boolean { BooleanE }
  | number  { NumberE }
  
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
    {-| '(' E ')' { NumParE $2 }-}
    | n { NumberLiteralE $1 }
    {-| fid '(' LV ')' { FuncE $1 $3 } -}

LV  : LVNV       { LVE $ listLVNVE $1 }
    | {-empty-}  { LVE [] }

LVNV    : E             { LVNVE [$1] }
        | E ',' LVNV    { LVNVE $ $1 : listLVNVE $3}

LD  : T DST ';' LD  { LDE $ ($1, listDSTE $2) : listLDE $4}
    | {-empty-}     { LDE [] }

DST : id '=' E          {DSTE [DeclVal $1 $3]}
    | id                {DSTE [Decl $1]}
    | id '=' E ',' DST  {DSTE $ DeclVal $1 $3 : listDSTE $5}
    | id ',' DST        {DSTE $ Decl $1 : listDSTE $3}
    
LI  : {-empty-}         { LIE [] }
    | I LI              { LIE $ $1 : listLIE $2}
    
LIR : {-empty-}         { LIRE [] }
    | IR LIR            { LIRE $ $1 : listLIRE $2}
    
    
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
    | writeln string LPW                    { WriteE $ PWSE $2 : listLPWE $3 }
    | writeln E LPW                         { WriteE $ PWEE $2 : listLPWE $3 }
    | read id ';'                           { ReadE $2 }
    
IR  : with LD do LIR end ';'                { WithDoRE $2 $4 }
    | repeat E times LIR end ';'            { RepeatRE $2 $4 }
    | fid '(' LV ')' ';'                    { FuncE $1 $3 }
    | id '=' E ';'                          { AssignE $1 $3 }
    | for id from E to E do LIR end ';'     { ForRE $2 $4 $6 $8 }
    | for id from E to E by E do LIR end ';'{ ForByRE $2 $4 $6 $8 $10 }
    | if E then LIR end ';'                 { IfThenRE $2 $4 }
    | if E then LIR else LIR end ';'        { IfThenElseRE $2 $4 $6 }
    | while E do LIR end ';'                { WhileRE $2 $4}
    | write string LPW                      { WriteE $ PWSE $2 : listLPWE $3 }
    | write E LPW                           { WriteE $ PWEE $2 : listLPWE $3 }
    | writeln string LPW                    { WriteE $ PWSE $2 : listLPWE $3 }
    | writeln E LPW                         { WriteE $ PWEE $2 : listLPWE $3 }
    | read id ';'                           { ReadE $2 }
    | return E ';'                             { ReturnE $2 }
    
    
LPW : ';'                               { LPWE [] }
    | ',' string LPW                    { LPWE $ PWSE $2 : listLPWE $3 }
    | ',' E LPW                         { LPWE $ PWEE $2 : listLPWE $3 }

{
data Exp =
    PE Exp Exp                          |
    LBLOCKE {listLBLOCKE :: [Exp]}      |
    LDFE {listLDFE :: [Exp]}            |
    DFE String Exp Exp                  |
    RDFE String Exp Exp Exp             |
    LPE {listLPE :: [(Exp, String)]}    |
    LPNVE {listLPNVE :: [(Exp, String)]}|
    BooleanE                            |
    NumberE                             |
    ExpA Exp                            |
    ExpB Exp                            |
    TrueE                               |
    FalseE                              |
    ParE Exp                            |
    ComparE Exp String Exp              |
    NotE Exp                            |
    IdE String                          |
    LogicE Exp String Exp               |
    FuncE String Exp                    |
    
    LVE {listLVE :: [Exp]}              |
    LVNVE {listLVNVE :: [Exp]}          |
    
    MinusE Exp                          |
    AritE Exp String Exp                |
    {-NumParE Exp                         |-}
    NumberLiteralE String               |
    
    
    LDE {listLDE :: [(Exp, [Exp])]}     |
    DSTE {listDSTE :: [Exp]}            |
    Decl String                         |
    DeclVal String Exp                  |
    
    
    LIE {listLIE :: [Exp]}              |
    LIRE {listLIRE :: [Exp]}            |
    
    
    WithDoE Exp Exp                     |
    RepeatE Exp Exp                     |
    AssignE String Exp                  |
    ForE String Exp Exp Exp             |
    ForByE String Exp Exp Exp Exp       |
    IfThenE Exp Exp                     |
    IfThenElseE Exp Exp Exp             |
    WhileE Exp Exp                      |
    WriteE {listWriteE :: [Exp]}        |
    ReadE String                        |
    
    
    WithDoRE Exp Exp                    |
    RepeatRE Exp Exp                    |
    ForRE String Exp Exp Exp            |
    ForByRE String Exp Exp Exp Exp      |
    IfThenRE Exp Exp                    |
    IfThenElseRE Exp Exp Exp            |
    WhileRE Exp Exp                     |
    
    ReturnE Exp                         |
    
    LPWE {listLPWE :: [Exp]}            |
    PWEE Exp                            |
    PWSE String 
    
    
	deriving Show


happyError :: [Token] -> a
happyError tks = error ("Parse error at " ++ lcn ++ "\n")
    where
        lcn =   case tks of
                    [] -> "end of file"
                    tk:_ -> "line " ++ show l ++ ", column " ++ show c
                        where
                            AlexPn _ l c = tokenPosn tk
}
