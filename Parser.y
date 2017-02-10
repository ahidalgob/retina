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
    '=='             { EqualTK _ }
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


Exp : program       { ProgramE }

{
data Exp =
	ProgramE
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
