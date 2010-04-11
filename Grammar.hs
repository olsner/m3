{-# LANGUAGE NoMonomorphismRestriction #-}

module Grammar (pUnit, pName) where

import Control.Applicative
import Control.Arrow
import Control.Functor.Fix

import Data.Maybe

import AST
import CppToken
import Parser

none p = not . any p
token t = satisfy ((== t) . snd)
lookToken t = satisfyLook ((== t) . snd)

pUnit = Unit <$> many pImport <*> (pModule <|> pFunction)

pModule = keyword "module" >> (Decl <$> pName) <* token Semicolon <*> (ModuleDef <$> many pFunction) <* eof

pImport = keyword "import" >> pName <* token Semicolon
pSimpleName = (\(_,nm) -> QualifiedName [nm]) <$> identifier
pName = QualifiedName . map snd <$> sepBy1 identifier (token DoubleColon)

pFunction = choice
  [(\ret nm params code -> Decl nm (FunctionDef ret params code)) <$>
    pType <*> pName <*> pFormalParamList <*> pCompoundStatement
  ,keyword "extern" *> (
    (\linkage ret nm params -> Decl nm (ExternalFunction (fmap snd linkage) ret params))
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
validateFormalParams [] = True
validateFormalParams xs = none (== VarargParam) (init xs)

infixl 3 $>
($>) = flip (<$)

pType = pArraySuffix =<< choice
  [keyword "void" $> TVoid
  ,keyword "int" $> TInt
  ,keyword "char" $> TChar
  ,keyword "const" >> (TConst <$> pType)
  ,inBrackets (TPtr <$> pType)
  ]
pArraySuffix t = optional (snd <$> inBrackets integer) >>= maybe (return t) (pArraySuffix . flip TArray t)

pStatement = choice $
  [token Semicolon $> EmptyStmt
  ,keyword "return" >> token Semicolon >> return ReturnStmtVoid
  ,keyword "return" *> (ReturnStmt <$> pExpression) <* token Semicolon
  ,ExprStmt <$> pExpression >>= \t -> t `seq` token Semicolon >> return t
  -- TODO Implement declarations of multiple variables in one statement
  ,flip VarDecl <$> pType <*> pName <*> (token Semicolon >> CompoundStmt <$> many pStatement <* lookToken CloseBrace)
  ,CompoundStmt <$> inBraces (many pStatement)
  ,keyword "if" *> (IfStmt <$> inParens pExpression <*> pStatement <*> (fromMaybe EmptyStmt <$> optional pElse))
  ]

pElse = keyword "else" >> pStatement

pExpression = pLeftExpression <**> pExpressionSuffix

pLeftExpression = choice
  [inParens pExpression
  ,InF . EVarRef <$> pName
  ,InF . EString . snd <$> string -- FIXME Position is thrown away
  ,InF . EInt . snd <$> integer -- FIXME Position is thrown away
  ]

pAssignmentOperator = token Assignment -- TODO Also handle operator-assignments, once lexer and token definitions have it.

eAssignment a b c = InF (EAssignment a b c)

pExpressionSuffix :: Parser Token (ExprF -> ExprF)
pExpressionSuffix = choice
  [flip <$> (eAssignment <$> pAssignmentOperator) <*> pExpression
  ,(InF .) <$> flip EFunCall <$> inParens (listOf pExpression)
  ,return id
  ]

keyword str = token (Identifier str) <|> token (Reserved str)
parseJust f = second (fromJust . f) <$> satisfy (isJust . f . snd)
identifier = parseJust fromIdentifier
integer = second fromIntegral <$> parseJust fromIntegerTok
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

