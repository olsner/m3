{-# LANGUAGE PatternGuards,DeriveDataTypeable #-}

module CppToken
  (Token
  ,Tok(..)
  
  ,oneCharOperators
  ,multiCharOperators
  ,postfixOperators
  ,unaryOperators

  ,reservedWords

  , module SourcePos
  ) where

import Data.Data (Data,Typeable)
import SourcePos

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
    | Modulo
    
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

    deriving (Read, Eq, Ord, Data, Typeable)

oneCharOperators = zip ";{}()[],.*&+-/!~|<>?:=%" $
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
    ,Assignment
    ,Modulo]

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
    ,"break"
    ,"cast"
    ,"char"
    ,"class"
    ,"const"
    ,"continue"
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
instance Show Tok where
  show token = '`':showTok' token ++ "'"

showTok' (Reserved word)    = word
showTok' (Identifier ident) = ident
showTok' (StringTok s)      = show s
showTok' (IntegerTok i)     = show i
showTok' (CharTok c)        = show c
showTok' token
  | Just c <- lookup token (map swap oneCharOperators) = [c]
  | Just str <- lookup token (map swap multiCharOperators) = str
