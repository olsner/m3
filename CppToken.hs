{-# LANGUAGE PatternGuards,DeriveDataTypeable #-}

module CppToken where

import Data.Data (Data,Typeable)

data SourcePos = SourcePos { sourceName :: String, sourceLine :: Int, sourceColumn :: Int } deriving (Ord,Eq,Data,Typeable)
instance Show SourcePos where
  show (SourcePos name line column) = name++" (line "++show line++", column "++show column++")"
initialPos source = SourcePos source 1 1

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
    | OpenTypeBracket
    | CloseTypeBracket
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
    | LogicalNot -- ^ the ! operator (logical not)
    | Complement -- ^ the ~ operator (bitwise not/complement)
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
    | PlusAssign
    
    | LeftShift
    | RightShift

    -- Pre- or postfix depending on parse
    | Decrement
    | Increment

    deriving (Show, Read, Eq, Ord, Data, Typeable)

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

multiCharOperators = postfixOperators++
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
    ,(">>",RightShift)
    ,("+=",PlusAssign)
    ,("<[",OpenTypeBracket)
    ,("]>",CloseTypeBracket)
    ]

postfixOperators =
    [("--",Decrement)
    ,("++",Increment)]

unaryOperators =
    [Asterix, Ampersand, Plus, Minus, LogicalNot, Complement]

reservedWords =
    ["bool"
    ,"cast"
    ,"char"
    ,"class"
    ,"const"
    ,"double"
    ,"else"
    ,"enum"
    ,"extern"
    ,"false"
    ,"float"
    ,"friend"
    ,"if"
    ,"import"
    ,"int"
    ,"long"
    ,"module"
    ,"namespace"
    ,"null"
    ,"return"
    ,"struct"
    ,"template"
    ,"true"
    ,"type"
    ,"typedef"
    ,"typename"
    ,"unsigned"
    ,"using"
    ,"void"
    ,"volatile"
    ,"while"]

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
