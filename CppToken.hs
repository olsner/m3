{-# LANGUAGE PatternGuards #-}

module CppToken where

import Text.ParserCombinators.Parsec.Pos

type Token = (SourcePos, Tok)
data Tok =
    Identifier String
    | Reserved String

    -- Literals
    | StringTok String
    | IntegerTok Integer
    | CharTok Char
    
    -- Punctuation (most of them without semantical meaning)
    | Semicolon
    | OpenBrace
    | CloseBrace
    | OpenParen
    | CloseParen
    | OpenBracket
    | CloseBracket
    | Comma
    
    | DoubleColon
    | Ellipsis
    -- Member access
    | Dot
    | Arrow
    -- Not sure if these should be individual operators
    {-
    | DotStar
    | ArrowStar
    -}
    
    -- Pointer, dereference or multiplication
    | Asterix
    -- Both non-shortcut/bitwise and, reference and address-of
    | Ampersand
    | Plus
    | Minus
    | Division
    
    -- Logic operators
    | ShortcutAnd
    | ShortcutOr
    | LogicalNot
    | Complement
    | Or
    
    | LessThan
    | GreaterThan
    | GreaterOrEqual
    | LessOrEqual
    | Equal
    | NotEqual
    
    -- Part one of ?:
    | QuestionMark
    -- Part two of :
    | SingleColon
    
    -- Assignment =
    | Assignment
    
    | LeftShift
    | RightShift
    deriving (Show, Read, Eq, Ord)

line n = setSourceLine (initialPos "hard-coded") n

oneCharOperators = zip ";{}()[],.*&+-/!~|<>?:=" $
    [Semicolon
    ,OpenBrace
    ,CloseBrace
    ,OpenParen
    ,CloseParen
    ,OpenBracket
    ,CloseBracket
    ,Comma
    ,Dot
    ,Asterix
    ,Ampersand
    ,Plus
    ,Minus
    ,Division
    ,LogicalNot
    ,Complement
    ,Or
    ,LessThan
    ,GreaterThan
    ,QuestionMark
    ,SingleColon
    ,Assignment]

multiCharOperators =
    [("&&",ShortcutAnd)
    ,("||",ShortcutOr)
    ,("::",DoubleColon)
    ,("...",Ellipsis)
    ,("->",Arrow)
    ,(">=",GreaterOrEqual)
    ,("<=",LessOrEqual)
    ,("==",Equal)
    ,("!=",NotEqual)
    ,("<<",LeftShift)
    ,(">>",RightShift)]

reservedWords =
    ["char"
    ,"class"
    ,"const"
    ,"double"
    ,"else"
    ,"enum"
    ,"extern"
    ,"float"
    ,"friend"
    ,"if"
    ,"int"
    ,"long"
    ,"namespace"
    ,"return"
    ,"struct"
    ,"template"
    ,"typedef"
    ,"typename"
    ,"unsigned"
    ,"using"
    ,"void"
    ,"volatile"]

swap (x,y) = (y,x)
printCanonically Semicolon = ";\n"
printCanonically token = (case token of
    _ | Just c <- lookup token (map swap oneCharOperators) -> [c]
    _ | Just str <- lookup token (map swap multiCharOperators) -> str
    Reserved word -> word
    Identifier ident -> ident
    StringTok s -> show s
    IntegerTok i -> show i
    _ -> "/*"++show token++"*/") ++ " "
