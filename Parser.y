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
    begin           { BeginTK _ }
    func            { FuncTK _ }
    repeat          { RepeatTK _ }
    program         { ProgramTK _ }
    woth            { WithTK _ }
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
    'div'           { DivExacTK _ }
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
    

%%

P : LDF program LI end ';' { PE $1 $3 }

LDF : DF LDF    { LDFE $ $1 : listLDFE $2 }
    |           {-empty-} {LDFE [] }

DF  : func fid '(' LP ')' begin LI end ';'          { DFE $2 $4 $7 }
    | func fid '(' LP ')' '->' T begin LIR end ';'  { RDFE $2 $4 $7 $9 }

LP : {-empty-}      { LPE [] }
   | LPNV           { LPE $ listLPNVE $1 }
   
LPNV : T id             { LPNVE [ParamE $1 $2] }
     | T id ',' LPNV    { LPNVE $ ParamE $1 $2 : listLPNVE $4 }

T : boolean { BooleanE }
  | number  { NumberE }
  
E  : EA { ExpA $1 }
   | EB { ExpB $1 }


EB : true { TrueE }
   | false { FalseE }
   | '(' EB ')' { BoolE $2 }
   | EA '<' EA { ComparE $1 "<" $3 }
   | EA '>' EA { ComparE $1 ">" $3 }
   | EA '<=' EA { ComparE $1 "<=" $3 }
   | EA '>=' EA { ComparE $1 ">=" $3 }
   | EA '==' EA { ComparE $1 "==" $3 }
   | EA '/=' EA { ComparE $1 "/=" $3 }
   | not EB { NotE $2 }
   | id { IdE $1 }
   | EB and EB { LogicE $1 "and" $3 }
   | EB or EB { LogicE $1 "or" $3 }
   | fid '(' LV ')' { FuncE $1 $3 }


EA : EA '+' EA { AritE $1 "+" $3 }
   {-| EA '-' EA { MinusE $1 $3 }
   | EA '*' EA { FactorE $1 $3 }
   | EA '%' EA { RestE $1 $3 }
   | EA 'mod' EA { ModE $1 $3 }
   | EA 'div' EA { DivE $1 $3 }
   | '(' EA ')' { ParenE $2 }
   | id { IdE $1 }
   | numberLiteral { NumberE $1 }
   | fid '(' LV ')' { FunArit $1 $3 }-}

LV  : LVNV       { LVE $ listLVNVE $1 }
    | {-empty-}  { LVE [] }

LVNV    : E             { LVNVE [$1] }
        | E ',' LVNV    { LVNVE $ $1 : listLVNVE $3}

LD  : T DST ';' LD     { LDE $ (DCTE $1 $ listDSTE $2) : listLDE $4}
    | {-empty-}     { LDE [] }

DST : id '=' E          {DSTE [DeclVal $1 $3]}
    | id                {DSTE [Decl $1]}
    | id '=' E ',' DST  {DSTE $ DeclVal $1 $3 : listDSTE $5}
    | id ',' DST        {DSTE $ Decl $1 : listDSTE $3}
    
LI  : {-empty-}         { LIE [] }
    | I LI              { LIE $ $1 : listLIE $2}
    
LIR : LI                { $1 }
    
I : while           { IE "hola"}
    
{
data Exp =
	PE Exp Exp                          |
    LDFE {listLDFE :: [Exp]}            |
    DFE String Exp Exp                  |
    RDFE String Exp Exp Exp             |
    LPE {listLPE :: [Exp]}              |
    LPNVE {listLPNVE :: [Exp]}          |
    ParamE Exp String                   |
    BooleanE                            |
    NumberE                             |
    ExpA Exp                            |
    ExpB Exp                            |
    TrueE                               |
    FalseE                              |
    BoolE Exp                           |
    ComparE Exp String Exp              |
    NotE Exp                            |
    IdE String                          |
    LogicE Exp String Exp               |
    FuncE String Exp                    |
    
    LVE {listLVE :: [Exp]}              |
    LVNVE {listLVNVE :: [Exp]}          |
    
    AritE Exp String Exp |
    
    LDE {listLDE :: [Exp]} |
    DCTE {t :: Exp, listDCTE :: [Exp]} |
    DSTE {listDSTE :: [Exp]} |
    Decl String |
    DeclVal String Exp |
    
    
    LIE {listLIE :: [Exp]}              |
    IE String
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
