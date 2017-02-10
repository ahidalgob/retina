
-- Segunda entrega del proyecto de Traductores
-- Lexer
-- Augusto Hidalgo 13-10665
-- Genesis Kufatty 13-10708



{
module Lexer(Token(..), printToken, runAlexScan, AlexUserState(..), AlexPosn(..)) where
}

%wrapper "monadUserState"

$digit = 0-9			-- digits
$Alpha = [a-zA-Z]		-- alphabetic characters
$alpha = [a-z]		    -- alphabetic characters
$ALPHA = [A-Z]		    -- alphabetic characters
$sim = [\=\+\-\*\/\/\=\%\=\=\<\>\>\=\<\=]

tokens :-
    $white+				                    ; --Desechar los espacios en blanco
    "#".*				                    ; --Desechar los comentarios
    
    
    
    --Tokens de palabras reservadas:
    while                                   {pushToken WhileTK}
    for                                     {pushToken ForTK}
    from                                    {pushToken FromTK}
    to                                      {pushToken ToTK}
    by                                      {pushToken ByTK}
    repeat                                  {pushToken RepeatTK}
    func                                    {pushToken FuncTK}
    return                                  {pushToken ReturnTK}
    begin                                   {pushToken BeginTK}
    program                                 {pushToken ProgramTK}
    with                                    {pushToken WithTK}
    do                                      {pushToken DoTK}
    end                                     {pushToken EndTK}
    times                                   {pushToken TimesTK}
    not                                     {pushToken NotTK}
    and                                     {pushToken AndTK}
    or                                      {pushToken OrTK}
    read                                    {pushToken ReadTK}
    write                                   {pushToken WriteTK}
    writeln                                 {pushToken WritelnTK}
    if                                      {pushToken IfTK}
    then                                    {pushToken ThenTK}
    else                                    {pushToken ElseTK}
    number                                  {pushToken NumberTK}
    boolean                                 {pushToken BooleanTK}
    true                                    {pushToken TrueTK}
    false                                   {pushToken FalseTK}
    mod                                     {pushToken ModTK}
    div                                     {pushToken DivTK}
    \,                                      {pushToken CommaTK}
    \(                                      {pushToken ParenOpenTK}
    \)                                      {pushToken ParenCloseTK}    
    \;                                      {pushToken Semicolon}
    \+                                      {pushToken PlusTK}
    \=\=                                    {pushToken EqualTK}
    \*                                      {pushToken ProductTK}
    \-                                      {pushToken MinusTK}
    \%                                      {pushToken RestTK}
    \/                                      {pushToken DivExacTK}
    \/\=                                    {pushToken DifTK}
    \>\=                                    {pushToken GreaterEqualTK}
    \<\=                                    {pushToken LessEqualTK}
    \>                                      {pushToken GreaterTK}
    \<                                      {pushToken LessTK}
    \-\>                                    {pushToken TypeTK}
    \=                                      {pushToken AssignTK}
    -- ($sim){3}                               {pushInvalid InvalidTK} -- Invalid Strings
    $digit+(\.[$digit]+)?	                {pushTokenWithString NumLiteralTK} -- Cadenas de Digitos     
    [$digit \.]+                            {pushInvalid InvalidTK} -- Invalid Numbers
    [a-z][a-zA-Z\_0-9]*	            	    {pushTokenWithString IdTK} -- Identificadores
    [a-z][a-zA-Z\_0-9]*\(	            	{pushFuncIdTK} -- Identificadores de funciones
    [$Alpha $digit \_]+                     {pushInvalid InvalidTK} -- Invalid Id
    \"([^\\\"\n]|\\\\|\\\"|\\n)*\"              {pushTokenWithString StringTK} -- Strings Correctos
    \"([^\"\n]|\\\"|\\n)*\"                     {pushInvalid InvalidTK} -- Invalid Strings
    \"([^\"\n]|\\\"|\\n)*                       {pushInvalid InvalidTK} -- Invalid Strings
    [a-zA-Z\_0-9]*\"([^\\\"\n]|\\\\|\\\"|\\n)*\"[a-zA-Z\_0-9]*  {pushInvalid InvalidTK} -- Invalid Strings
    .                                       {pushInvalid InvalidTK} -- npi
{


-- The token type:
data Token =
    WhileTK { tokenPosn :: AlexPosn }                               |
    ForTK { tokenPosn :: AlexPosn }                                 |
    FromTK { tokenPosn :: AlexPosn }                                |
    ToTK { tokenPosn :: AlexPosn }                                  |
    ByTK { tokenPosn :: AlexPosn }                                  |
    BeginTK { tokenPosn :: AlexPosn }                               |
    FuncTK { tokenPosn :: AlexPosn }                                |
    ReturnTK {tokenPosn :: AlexPosn}                                |
    RepeatTK { tokenPosn :: AlexPosn }                              |
    ProgramTK { tokenPosn :: AlexPosn }                             |
    WithTK { tokenPosn :: AlexPosn }                                |
    DoTK { tokenPosn :: AlexPosn }                                  |
    EndTK { tokenPosn :: AlexPosn }                                 |
    TimesTK { tokenPosn :: AlexPosn }                               |
    NotTK { tokenPosn :: AlexPosn }                                 |
    AndTK { tokenPosn :: AlexPosn }                                 |
    OrTK { tokenPosn :: AlexPosn }                                  |
    ReadTK { tokenPosn :: AlexPosn }                                |
    WriteTK { tokenPosn :: AlexPosn }                               |
    WritelnTK { tokenPosn :: AlexPosn }                             |
    IfTK { tokenPosn :: AlexPosn }                                  |
    ThenTK { tokenPosn :: AlexPosn }                                |
    ElseTK { tokenPosn :: AlexPosn }                                |
    NumberTK { tokenPosn :: AlexPosn }                              |
    BooleanTK { tokenPosn :: AlexPosn }                             |
    TrueTK { tokenPosn :: AlexPosn }                                |
    FalseTK { tokenPosn :: AlexPosn }                               |
    DivTK { tokenPosn :: AlexPosn }                                 |
    ModTK { tokenPosn :: AlexPosn }                                 |
    CommaTK { tokenPosn :: AlexPosn }                               |
    NumLiteralTK { tokenPosn :: AlexPosn, tokenString :: String}    |
    StringTK { tokenPosn :: AlexPosn, tokenString :: String}        |
    IdTK { tokenPosn :: AlexPosn, tokenString :: String}            |
    FuncIdTK { tokenPosn :: AlexPosn, tokenString :: String}        |
    PlusTK { tokenPosn :: AlexPosn }                                |
    EqualTK { tokenPosn :: AlexPosn }                               |
    ProductTK { tokenPosn :: AlexPosn }                             |
    MinusTK { tokenPosn :: AlexPosn }                               |
    RestTK { tokenPosn :: AlexPosn }                                |
    DivExacTK { tokenPosn :: AlexPosn }                             |
    DifTK  { tokenPosn :: AlexPosn }                                |
    GreaterEqualTK { tokenPosn :: AlexPosn }                        |
    LessEqualTK { tokenPosn :: AlexPosn }                           |
    GreaterTK { tokenPosn :: AlexPosn }                             |
    LessTK { tokenPosn :: AlexPosn }                                |
    AssignTK { tokenPosn :: AlexPosn }                              |
    ParenOpenTK { tokenPosn :: AlexPosn }                           |
    ParenCloseTK { tokenPosn :: AlexPosn }                          |
    Semicolon { tokenPosn :: AlexPosn }                             |
    TypeTK { tokenPosn :: AlexPosn }                                |
    InvalidTK { tokenPosn :: AlexPosn, tokenString :: String}
    deriving (Eq,Show)
    
printToken (TypeTK (AlexPn _ ln cn))           = printFoundedToken ln cn  (": signo '->'")
printToken (CommaTK (AlexPn _ ln cn))          = printFoundedToken ln cn  (": signo ','")
printToken (ParenOpenTK (AlexPn _ ln cn))      = printFoundedToken ln cn  (": signo '('")
printToken (ParenCloseTK (AlexPn _ ln cn))     = printFoundedToken ln cn  (": signo ')'")
printToken (Semicolon (AlexPn _ ln cn))        = printFoundedToken ln cn  (": signo ';'")
printToken (PlusTK (AlexPn _ ln cn))           = printFoundedToken ln cn  (": signo '+'")
printToken (EqualTK (AlexPn _ ln cn))          = printFoundedToken ln cn  (": signo '=='")
printToken (ProductTK (AlexPn _ ln cn))        = printFoundedToken ln cn  (": signo '*'")
printToken (MinusTK (AlexPn _ ln cn))          = printFoundedToken ln cn  (": signo '-'")
printToken (RestTK (AlexPn _ ln cn))           = printFoundedToken ln cn  (": signo '%'")
printToken (DifTK (AlexPn _ ln cn))            = printFoundedToken ln cn  (": signo '/='")
printToken (GreaterEqualTK (AlexPn _ ln cn))   = printFoundedToken ln cn  (": signo '>='")
printToken (LessEqualTK (AlexPn _ ln cn))      = printFoundedToken ln cn  (": signo '<='")
printToken (GreaterTK (AlexPn _ ln cn))        = printFoundedToken ln cn  (": signo '>'")
printToken (LessTK (AlexPn _ ln cn))           = printFoundedToken ln cn  (": signo '<'")
printToken (DivExacTK (AlexPn _ ln cn))        = printFoundedToken ln cn  (": signo '/'")
printToken (AssignTK (AlexPn _ ln cn))         = printFoundedToken ln cn  (": signo '='")
printToken (ToTK (AlexPn _ ln cn))             = printFoundedToken ln cn  (": palabra reservada 'to'")
printToken (ByTK (AlexPn _ ln cn))             = printFoundedToken ln cn  (": palabra reservada 'by'")
printToken (ForTK (AlexPn _ ln cn))            = printFoundedToken ln cn  (": palabra reservada 'for'")
printToken (WhileTK (AlexPn _ ln cn))          = printFoundedToken ln cn  (": palabra reservada 'while'")
printToken (BeginTK (AlexPn _ ln cn))          = printFoundedToken ln cn  (": palabra reservada 'begin'")
printToken (FuncTK (AlexPn _ ln cn))           = printFoundedToken ln cn  (": palabra reservada 'func'")
printToken (ReturnTK (AlexPn _ ln cn))         = printFoundedToken ln cn  (": palabra reservada 'return'")
printToken (RepeatTK (AlexPn _ ln cn))         = printFoundedToken ln cn  (": palabra reservada 'repeat'")
printToken (ProgramTK (AlexPn _ ln cn))        = printFoundedToken ln cn  (": palabra reservada 'program'")
printToken (WithTK (AlexPn _ ln cn))           = printFoundedToken ln cn  (": palabra reservada 'with'")
printToken (DoTK (AlexPn _ ln cn))             = printFoundedToken ln cn  (": palabra reservada 'do'")
printToken (EndTK (AlexPn _ ln cn))            = printFoundedToken ln cn  (": palabra reservada 'end'")
printToken (TimesTK (AlexPn _ ln cn))          = printFoundedToken ln cn  (": palabra reservada 'times'")
printToken (NotTK (AlexPn _ ln cn))            = printFoundedToken ln cn  (": palabra reservada 'not'")
printToken (AndTK (AlexPn _ ln cn))            = printFoundedToken ln cn  (": palabra reservada 'and'")
printToken (OrTK (AlexPn _ ln cn))             = printFoundedToken ln cn  (": palabra reservada 'or'")
printToken (ReadTK (AlexPn _ ln cn))           = printFoundedToken ln cn  (": palabra reservada 'read'")
printToken (FromTK (AlexPn _ ln cn))           = printFoundedToken ln cn  (": palabra reservada 'from'")
printToken (WriteTK (AlexPn _ ln cn))          = printFoundedToken ln cn  (": palabra reservada 'write'")
printToken (WritelnTK (AlexPn _ ln cn))        = printFoundedToken ln cn  (": palabra reservada 'writeln'")
printToken (IfTK (AlexPn _ ln cn))             = printFoundedToken ln cn  (": palabra reservada 'if'")
printToken (ThenTK (AlexPn _ ln cn))           = printFoundedToken ln cn  (": palabra reservada 'then'")
printToken (ElseTK (AlexPn _ ln cn))           = printFoundedToken ln cn  (": palabra reservada 'else'")
printToken (NumberTK (AlexPn _ ln cn))         = printFoundedToken ln cn  (": tipo de dato 'number'")
printToken (BooleanTK (AlexPn _ ln cn))        = printFoundedToken ln cn  (": tipo de dato 'boolean'")
printToken (TrueTK (AlexPn _ ln cn))           = printFoundedToken ln cn  (": literal booleano 'true'")
printToken (FalseTK (AlexPn _ ln cn))          = printFoundedToken ln cn  (": literal booleano 'false'")
printToken (DivTK (AlexPn _ ln cn))            = printFoundedToken ln cn  (": palabra reservada 'div'")
printToken (ModTK (AlexPn _ ln cn))            = printFoundedToken ln cn  (": palabra reservada 'mod'")
printToken (NumLiteralTK (AlexPn _ ln cn) s)   = printFoundedToken ln cn  (": literal numerico '"++ s ++ "'")
printToken (StringTK (AlexPn _ ln cn) s)       = printFoundedToken ln cn  (": literal de string "++ s )
printToken (IdTK (AlexPn _ ln cn) s)           = printFoundedToken ln cn  (": identificador '"++ s ++ "'")
printToken (FuncIdTK (AlexPn _ ln cn) s)       = printFoundedToken ln cn  (": identificador de funcion '"++ s ++ "'")
printToken (InvalidTK (AlexPn _ ln cn) s)      = printFoundedToken ln cn  (": input no reconocido '"++ s ++ "'")

printFoundedToken ln cn s = putStrLn("linea " ++ (show ln) ++", columna " ++ (show cn) ++ s)


alexEOF :: Alex ()
alexEOF = return ()

data AlexUserState = AlexUserState { tokenList :: [Token], invalidTokens :: Bool}
alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState [] False

modifyUserState :: (AlexUserState -> AlexUserState) -> Alex ()
modifyUserState f = Alex $ \s ->    let current = alex_ust s
                                        new     = f current
                                    in
                                        Right (s { alex_ust = new },())

getUserState ::  Alex AlexUserState
getUserState = Alex $ \s -> Right (s,alex_ust s)

pushValid :: Token -> AlexUserState -> AlexUserState
pushValid tkn ts =  if invalidTokens ts
                        then ts
                        else ts{tokenList = tkn : (tokenList ts)}

pushToken :: (AlexPosn -> Token) -> AlexAction ()
pushToken tokenizer =
    \(posn,prevChar,pending,s) len -> modifyUserState (pushValid (tokenizer posn)) >> alexMonadScan

pushTokenWithString :: (AlexPosn -> String -> Token) -> AlexAction ()
pushTokenWithString tokenizer =
    \(posn,prevChar,pending,s) len -> modifyUserState (pushValid $ tokenizer posn $ take len s) >> alexMonadScan
    
pushFuncIdTK :: AlexAction ()
pushFuncIdTK = 
     \(posn@(AlexPn x ln cn),prevChar,pending,s) len -> do  modifyUserState (pushValid $ FuncIdTK posn $ take (len-1) s)
                                                            modifyUserState (pushValid $ ParenOpenTK (AlexPn x ln (cn+len-1)))
                                                            alexMonadScan
                                    
pushInvalid :: (AlexPosn -> String -> Token) -> AlexAction ()
pushInvalid tokenizer =
    \(posn,prevChar,pending,s) len -> modifyUserState checkInvalid >> modifyUserState (push posn $ take len s) >> alexMonadScan
        where
            push :: AlexPosn -> String -> AlexUserState -> AlexUserState
            push posn ss ts = ts{tokenList = (tokenizer posn ss):(tokenList ts)}
            checkInvalid :: AlexUserState -> AlexUserState
            checkInvalid ts =   if not $ invalidTokens ts 
                                    then AlexUserState [] True
                                    else ts
            
runAlexScan :: String -> Either String AlexUserState
runAlexScan s = runAlex s $ alexMonadScan >> getUserState

}
