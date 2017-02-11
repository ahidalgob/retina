
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
    \;                                      {pushToken SemicolonTK}
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
    SemicolonTK { tokenPosn :: AlexPosn }                             |
    TypeTK { tokenPosn :: AlexPosn }                                |
    NumLiteralTK { tokenPosn :: AlexPosn, tokenString :: String}    |
    StringTK { tokenPosn :: AlexPosn, tokenString :: String}        |
    IdTK { tokenPosn :: AlexPosn, tokenString :: String}            |
    FuncIdTK { tokenPosn :: AlexPosn, tokenString :: String}        |
    InvalidTK { tokenPosn :: AlexPosn, tokenString :: String}
    deriving (Eq)

instance Show Token where
    show (WhileTK _)        = "palabra reservada 'while'"
    show (ForTK _)          = "palabra reservada 'for'"
    show (FromTK _)         = "palabra reservada 'from'"
    show (ToTK _)           = "palabra reservada 'to'"
    show (ByTK _)           = "palabra reservada 'by'"
    show (BeginTK _)        = "palabra reservada 'begin'"
    show (FuncTK _)         = "palabra reservada 'func'"
    show (ReturnTK _)       = "palabra reservada 'return'"
    show (RepeatTK _)       = "palabra reservada 'repeat'"
    show (ProgramTK _)      = "palabra reservada 'program'"
    show (WithTK _)         = "palabra reservada 'with'"
    show (DoTK _)           = "palabra reservada 'do'"
    show (EndTK _)          = "palabra reservada 'end'"
    show (TimesTK _)        = "palabra reservada 'times'"
    show (NotTK _)          = "palabra reservada 'not'"
    show (AndTK _)          = "palabra reservada 'and'"
    show (OrTK _)           = "palabra reservada 'or'"
    show (ReadTK _)         = "palabra reservada 'read'"
    show (WriteTK _)        = "palabra reservada 'write'"
    show (WritelnTK _)      = "palabra reservada 'writeln'"
    show (IfTK _)           = "palabra reservada 'if'"
    show (ThenTK _)         = "palabra reservada 'then'"
    show (ElseTK _)         = "palabra reservada 'else'"
    show (NumberTK _)       = "tipo de dato 'number'"
    show (BooleanTK _)      = "tipo de dato 'boolean'"
    show (TrueTK _)         = "literal booleano 'true'"
    show (FalseTK _)        = "literal booleano 'false'"
    show (DivTK _)          = "palabra reservada 'div'"
    show (ModTK _)          = "palabra reservada 'mod'"
    show (CommaTK _)        = "signo ','"
    show (PlusTK _)         = "signo '+'"
    show (EqualTK _)        = "signo '=='"
    show (ProductTK _)      = "signo '*'"
    show (MinusTK _)        = "signo '-'"
    show (RestTK _)         = "signo '%'"
    show (DivExacTK _)      = "signo '/'"
    show (DifTK _)          = "signo '/='"
    show (GreaterEqualTK _) = "signo '>='"
    show (LessEqualTK _)    = "signo '<='"
    show (GreaterTK _)      = "signo '>'"
    show (LessTK _)         = "signo '<'"
    show (AssignTK _)       = "signo '='"
    show (ParenOpenTK _)    = "signo '('"
    show (ParenCloseTK _)   = "signo ')'"
    show (SemicolonTK _)    = "signo ';'"
    show (TypeTK _)         = "signo '->'"
    show (NumLiteralTK _ s) = "literal numerico '"++ s ++ "'"
    show (StringTK _ s)     = "literal de string "++ s
    show (IdTK _ s)         = "identificador '"++ s ++ "'"
    show (FuncIdTK _ s)     = "identificador de funcion '"++ s ++ "'"
    show (InvalidTK _ s)    = "input no reconocido '"++ s ++ "'"


printToken tk = putStrLn $ "linea " ++ (show ln) ++", columna " ++ (show cn) ++ ": " ++ show tk
    where
        AlexPn _ ln cn = tokenPosn tk


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
