{-# LANGUAGE NoMonomorphismRestriction #-}

module CppLexer (lexCpp) where

import Control.Applicative --hiding ((<|>),many)
import Control.Arrow
import Control.Monad

import Data.Char
import Data.Maybe

import Numeric

--import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos
import Parser

-- Token, Tok
import CppToken

--type Lexer a = GenParser Char () a
type Lexer a = Parser Char a

x `over` y = x <* y
(.:) = (.).(.)

oneOf = choice . map char
char = token
getPosition = return (initialPos "")
try = id
type ParseError = String
{-next = satisfy (const True)
match xs = try (match' xs)
match' [] = return []
match' (x:xs) = liftM2 (:) (satisfy (== x)) (match' xs)-}

lexCpp :: String -> String -> Either ParseError [Token]
lexCpp filename source = Right $ fst $ runParser file source
--lexCpp filename source = runParser file () "cmd-line" source
	where
		--position = setSourceColumn (initialPos filename) 0
		position = initialPos filename

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
	pos <- saveTokPos (char '#' >> fmap StringTok takeRestOfLine)
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
escapedChar q = choice [char '\\' >> fmap unescape anychar, satisfy (/= q)]
-- FIXME this uses haskell's unescaping which might very well be different from C/C++'s
unescape c = read ("\'\\" ++ c : "\'")

fromReadS :: ReadS a -> String -> a
fromReadS f = fst . head . f

hexOrOctal = char '0' >> choice [hexNumber, octalNumber]

hexNumber = do
	satisfy (`elem` "xX")
	ds <- many1 (satisfy isHexDigit)
	return (fromReadS readHex ds)
octalNumber = do
	ds <- many1 (satisfy (liftM2 (&&) (>= '0') (<= '7')))
	return (fromReadS readOct ds)
decimalNumber = fmap read $ many1 (satisfy isDigit)

integerLiteral = do
	signF <- choice [char '-' >> return negate, return id]
	i <- choice [hexOrOctal,decimalNumber]
	return (IntegerTok . signF $ i)
