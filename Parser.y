{
module Parser where

import Lexer 
}

%name parser 
%tokentype { Token }
%error { parserError } 

%token 
    num         { TokenNum $$ }
    '+'         { TokenAdd }
    "&&"        { TokenAnd }
    true        { TokenTrue }
    false       { TokenFalse }
    if          { TokenIf }
    then        { TokenThen }
    else        { TokenElse }
    var         { TokenVar $$ }
    '\\'        { TokenLam }
    "->"        { TokenArrow }
    '('         { TokenLParen }
    ')'         { TokenRParen }
    '='         { TokenEq }
    let         { TokenLet }
    in          { TokenIn }
    Bool        { TokenBoolean }
    Num         { TokenNumber }
    ':'         { TokenColon }
    '-'         { TokenSub }
    '*'         { TokenMult }
    "||"        { TokenOr }
    '>'         { TokenGreater }
    "=="        { TokenEquals }
    where       { TokenWhere }
    

%%

Exp         : num                           { Num $1 }
            | true                          { BTrue }
            | false                         { BFalse }
            | Exp '+' Exp                   { Add $1 $3 }
            | Exp "&&" Exp                  { And $1 $3 }
            | if Exp then Exp else Exp      { If $2 $4 $6 }
            | var                           { Var $1 }
            | '\\' var ':' Type "->" Exp    { Lam $2 $4 $6 }
            | Exp Exp                       { App $1 $2 }
            | '(' Exp ')'                   { Paren $2 }
            | let var '=' Exp in Exp        { Let $2 $4 $6 }
            | Exp '-' Exp                   { Sub $1 $3 }
            | Exp '*' Exp                   { Mult $1 $3 }
            | Exp "||" Exp                  { Or $1 $3 }
            | Exp '>' Exp                   { Greater $1 $3 }
            | Exp "==" Exp                  { Equals $1 $3 }
            | '(' Exp ')' where var '=' Exp { Where $5 $7 $2 }

Type        : Bool                          { TBool }
            | Num                           { TNum }
            | '(' Type "->" Type ')'        { TFun $2 $4 }

{

parserError :: [Token] -> a 
parserError _ = error "Syntax error!"

}
