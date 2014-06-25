{-# LANGUAGE NoMonomorphismRestriction,CPP #-}

#ifndef USE_PARSEC
#define USE_PARSEC 1
#endif

module CppLexer (lexCpp) where

import Control.Applicative hiding ((<|>),many)
import Control.Arrow
import Control.Monad

import Data.Char
import Data.Maybe

import Numeric

-- Token, Tok
import CppToken

#if USE_PARSEC
import Text.ParserCombinators.Parsec hiding (sourceName,sourceLine,sourceColumn,getPosition)
import qualified Text.ParserCombinators.Parsec as P
#else
import Parser
import Control.Applicative ((<|>),many)
#endif

#if USE_PARSEC
type Lexer a = GenParser Char () a
next = satisfy (const True)
match xs = try (match' xs)
match' [] = return []
match' (x:xs) = liftM2 (:) (satisfy (== x)) (match' xs)

instance Applicative (GenParser t s) where
  (<*>) = ap
  pure = return

getPosition = (liftM3 SourcePos P.sourceName P.sourceLine P.sourceColumn) <$> P.getPosition
#else
type Lexer a = Parser Char a
oneOf = choice . map char
char = token
getPosition = return (initialPos "")
try = id
type ParseError = String
#endif

lexCpp :: String -> String -> Either ParseError [Token]
#if USE_PARSEC
lexCpp filename source = runParser file () filename source
#else
lexCpp filename source = Right $ fst $ runParser file source
#endif

file = catMaybes <$> many cppToken

cppToken :: Lexer (Maybe Token)
cppToken = choice
	[comment
	,operator
	,identifierOrReserved
	,literal
	,preprocLine
	,whiteSpace
	,saveTokPos (fmap (StringTok . (:[])) anychar)]

saveTokPos :: Lexer Tok -> Lexer (Maybe Token)
saveTokPos m = getPosition >>= \pos -> m >>= \t -> return (Just (pos,t))

charTok :: (Char,Char -> a) -> Lexer a
charTok (c,f) = fmap f (char c)
anychar = next

-- TODO Handle escaped newlines (should we take note of escaped newlines in comments!?)
takeRestOfLine = many (satisfy (/= '\n')) <* match "\n"

ignore p = p >> return Nothing

preprocLine = ignore $ try $ do
	pos <- getPosition
	unless (sourceColumn pos == 1) (fail "Retry other token")
	many whiteSpace
	-- Save position of '#', not of the beginning of the line
	_pos <- saveTokPos (char '#' >> fmap StringTok takeRestOfLine)
	-- Since they are mostly spam, ignore preprocLines for now
	-- We should take in # line file-lines and update SourcePos with the information
	return ()

whiteSpace = ignore $ oneOf " \t\n"
comment = char '/' >> ignore (choice . map try $
	[char '*' <* cCommentTail
	,char '/' <* takeRestOfLine])

cCommentTail = choice
	[char '*' >> (ignore (char '/') <|> cCommentTail)
	,many1 (satisfy (/= '*')) >> cCommentTail
	,fail "Unterminated comment" >> ignore (many anychar)]

operator = saveTokPos (choice [try multiCharOperator,oneCharOperator])
multiCharOperator = choice (map (\(s,tok) -> match s >> return tok) multiCharOperators)
oneCharOperator = choice (map (charTok . second const) oneCharOperators)

identifierOrReserved = try $ saveTokPos identifierOrReservedTok
identifierOrReservedTok = do
	name <- identString
	if name `elem` reservedWords
		then return (Reserved name)
		else return (Identifier name)
	where
		reservedWords = CppToken.reservedWords
		identString = liftM2 (:) (satisfy (`elem` initials)) (many (satisfy (`elem` follows)))
		initials = ['a'..'z']++['A'..'Z']++['_']
		follows = ['0'..'9']++initials

literal = saveTokPos (choice [stringLiteral,integerLiteral,try charLiteral,appleCharLiteral])

charLiteral = do
	char '\''
	c <- escapedChar '\''
	char '\''
	return (CharTok c)

appleCharLiteral = do
	char '\''
	cs <- many (escapedChar '\'') -- actually exactly four, but why be picky when we're not actually parsing them properly anyway
	char '\''
	return (StringTok cs) -- TODO Parse as four-character-code and build an integer

stringLiteral = do
	char '\"'
	str <- many (escapedChar '\"')
	char '\"'
	return (StringTok str)
escapedChar q = choice [char '\\' >> anychar >>= unescape, satisfy (/= q)]
-- FIXME this uses haskell's unescaping which is different from C/C++'s
unescape :: MonadPlus m => Char -> m Char
unescape c = case c of
  'n' -> return '\n'
  't' -> return '\t'
  '"' -> return c
  _ -> fail ("Unrecognized escaped character: '\\"++[c]++"'")

fromReadS :: ReadS a -> String -> a
fromReadS f = fst . head . f

hexOrOctal = char '0' >> choice [hexNumber, octalNumber]

hexNumber = do
	satisfy (`elem` "xX")
	ds <- many1 (satisfy isHexDigit)
	return (fromReadS readHex ds)
octalNumber = do
	ds <- many (satisfy (liftM2 (&&) (>= '0') (<= '7')))
	return (fromReadS readOct ('0':ds))
decimalNumber = fmap read $ many1 (satisfy isDigit)

integerLiteral = do
	signF <- choice [char '-' >> return negate, return id]
	i <- choice [hexOrOctal,decimalNumber]
	return (IntegerTok . signF $ i)
