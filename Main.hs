{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Applicative
import Control.Functor.Fix

import Data.Maybe

import AST
import CppToken
import CppLexer (lexCpp)
import Parser
import CodeGen
import TypeCheck (typecheck)

none p = not . any p

pUnit = Unit <$> many pImport <*> (pModule <|> pFunction)

pModule = keyword "module" >> (Decl <$> pName) <* token Semicolon <*> (ModuleDef <$> many pFunction) <* eof

pImport = keyword "import" >> pName <* token Semicolon
pSimpleName = StringName <$> identifier
pName = QualifiedName <$> sepBy1 identifier (token DoubleColon)

pFunction = choice
  [(\ret nm params code -> Decl nm (FunctionDef ret params code)) <$>
    pType <*> pName <*> pFormalParamList <*> pCompoundStatement
  ,keyword "extern" *> (
    (\linkage ret nm params -> Decl nm (ExternalFunction linkage ret params))
      <$> optional string <*> pType <*> pName <*> pFormalParamList
      <* token Semicolon)
  ]

pFormalParamList = do
  xs <- inParens . listOf . choice $ [pFormalParam, pVarargParam]
  guardMsg (validateFormalParams xs) "Invalid formal parameter list - vararg ellipsis must be last parameter."
  return xs
pCompoundStatement = inBraces (many pStatement)

pFormalParam = FormalParam <$> pType <*> optional pSimpleName
pVarargParam = token Ellipsis >> return VarargParam
validateFormalParams (x:xs) = none (== VarargParam) (init xs)
validateFormalParams [] = True

infixl 3 $>
($>) = flip (<$)

pType = pArraySuffix =<< choice
  [keyword "void" $> TVoid
  ,keyword "int" $> TInt
  ,keyword "char" $> TChar
  ,keyword "const" >> (TConst <$> pType)
  ,inBrackets (TPtr <$> pType)
  ]
pArraySuffix t = optional (inBrackets integer) >>= maybe (return t) (pArraySuffix . flip TArray t)

pStatement = choice $
  [token Semicolon $> EmptyStmt
  ,keyword "return" >> token Semicolon >> return ReturnStmtVoid
  ,keyword "return" *> (ReturnStmt <$> pExpression) <* token Semicolon
  ,ExprStmt <$> pExpression >>= \t -> t `seq` token Semicolon >> return t
  -- TODO Implement declarations of multiple variables in one statement
  ,flip VarDecl <$> pType <*> pName <*> (token Semicolon >> CompoundStmt <$> many pStatement <* lookToken CloseBrace)
  ,CompoundStmt <$> inBraces (many pStatement)
  ]

pExpression = pLeftExpression <**> pExpressionSuffix

pLeftExpression = choice
  [inParens pExpression
  ,InF . EVarRef <$> pName
  ,InF . EString <$> string
  ,InF . EInt <$> integer
  ]

pAssignmentOperator = token Assignment -- TODO Also handle operator-assignments, once lexer and token definitions have it.

eAssignment a b c = InF (EAssignment a b c)

pExpressionSuffix :: Parser Tok (ExprF -> ExprF)
pExpressionSuffix = choice
  [flip <$> (eAssignment <$> pAssignmentOperator) <*> pExpression
  ,(InF .) <$> flip EFunCall <$> inParens (listOf pExpression)
  ,return id
  ]

keyword str = token (Identifier str) <|> token (Reserved str)
parseJust f = fromJust . f <$> satisfy (isJust . f)
identifier = parseJust fromIdentifier
integer = fromIntegral <$> parseJust fromIntegerTok
string = parseJust fromStringTok

fromIdentifier (Identifier s) = Just s
fromIdentifier (Reserved s) = Just s
fromIdentifier _ = Nothing

fromIntegerTok (IntegerTok i) = Just i
fromIntegerTok _ = Nothing

fromStringTok (StringTok s) = Just s
fromStringTok _ = Nothing

inBraces p = token OpenBrace *> p <* token CloseBrace
inBrackets p = token OpenBracket *> p <* token CloseBracket
inParens p = token OpenParen *> p <* token CloseParen

listOf p = sepBy p (token Comma)

parse path = do
  input <- readFile path
  let res = lexCpp path input
  --print res
  let Right tokens = res
  --mapM_ print (map snd tokens)
  return (fst $ runParser pUnit (map snd tokens))

process :: Name -> Unit ExprF -> IO ()
process name ast = do
  print ast
  let ast' = typecheck name ast
  print ast'
  printLLVM name ast'

main = do
  process (QualifiedName ["ex1"]) =<< parse "tests/ex1.m"
  process (QualifiedName ["ex2"]) =<< parse "tests/ex2.m"
  process (QualifiedName ["std","io"]) =<< parse "tests/std/io.m"
