{-# LANGUAGE NoMonomorphismRestriction #-}

module Grammar.Utils where

import Control.Applicative
import Control.Arrow (second)

import Data.Maybe

import AST
import CppToken
import Grammar.Parser

infixl 3 $>
($>) = flip (<$)

token t = satisfy (show t) ((== t) . snd)
lookToken t = satisfyLook (show t) ((== t) . snd)

keyword str = token (Reserved str)
parseJust msg f = second (fromJust . f) <$> satisfy msg (isJust . f . snd)
identifier = parseJust "Identifier" fromIdentifier
integer = second fromIntegral <$> parseJust "integer" fromIntegerTok
string = parseJust "string" fromStringTok
char = parseJust "char" fromCharTok

fromIdentifier (Identifier s) = Just s
fromIdentifier _ = Nothing

fromIntegerTok (IntegerTok i) = Just i
fromIntegerTok _ = Nothing

fromStringTok (StringTok s) = Just s
fromStringTok _ = Nothing

fromCharTok (CharTok c) = Just c
fromCharTok _ = Nothing

postfixOperator = choice (map (token.snd) postfixOperators)

inBraces p = token OpenBrace *> p <* token CloseBrace
inBrackets p = token OpenBracket *> p <* token CloseBracket
inParens p = token OpenParen *> p <* token CloseParen
inTypeBrackets p = token OpenTypeBracket *> p <* token CloseTypeBracket

listOf p = sepBy p (token Comma)

pSimpleName = (\(_,nm) -> QualifiedName [nm]) <$> identifier
pName = QualifiedName . map snd <$> sepBy1 identifier (token DoubleColon)

