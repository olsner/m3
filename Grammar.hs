{-# LANGUAGE NoMonomorphismRestriction #-}

module Grammar (pUnit, pName) where

import Control.Applicative
import Control.Arrow
import Control.Functor.Fix

import Data.Maybe

import AST
import CppToken
import Parser

token t = satisfy ((== t) . snd)
lookToken t = satisfyLook ((== t) . snd)

pUnit = Unit <$> many pImport <*> (pModule <|> pFunction) <* eof

pModule = keyword "module" *> (Decl <$> pName) <* token Semicolon <*> (ModuleDef <$> many pFunction) <* eof

pImport :: Parser Token Name
pImport = keyword "import" *> pName <* token Semicolon
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

pFormalParamList = inParens . listOf $ choice [pFormalParam, pVarargParam]
  -- TODO Move somewhere - typechecker?
  -- guardMsg (validateFormalParams xs) "Invalid formal parameter list - vararg ellipsis must be last parameter."
  -- return xs

pFormalParam = FormalParam <$> pType <*> optional pSimpleName
pVarargParam = VarargParam <$ token Ellipsis

infixl 3 $>
($>) = flip (<$)

pType = pLeftType <**> pArraySuffix
pLeftType = choice
  [keyword "void" $> TVoid
  ,keyword "int" $> TInt
  ,keyword "char" $> TChar
  ,keyword "const" *> (TConst <$> pType)
  ,inBrackets (TPtr <$> pType)
  ]
pArraySuffix = maybe id TArray <$> optional (snd <$> inBrackets integer)

forceThenSemicolon t = t `seq` token Semicolon >> return t
pCompoundStatement = inBraces (many pStatement)
pStatement = choice $
  [token Semicolon $> EmptyStmt
  ,ReturnStmtVoid <$ (keyword "return" *> token Semicolon)
  ,ReturnStmt <$> (keyword "return" *> pExpression <* token Semicolon)
  ,ExprStmt <$> pExpression <* token Semicolon
  -- TODO Implement declarations of multiple variables in one statement
  -- Note: This syntax defines the scope of a variable as "everything until the next closing brace. I this (really) correct?
  -- TODO: What happens with e.g. if (foo) type var; !?
  ,mkVarDecl <$> pType <*> sepBy1 ((,) <$> pName <*> optional pVarInitializer) (token Comma) <*> (token Semicolon *> (CompoundStmt <$> many pStatement <* lookToken CloseBrace))
  ,CompoundStmt <$> pCompoundStatement
  ,keyword "if" *> (IfStmt <$> inParens pExpression <*> pStatement <*> (fromMaybe EmptyStmt <$> optional pElse))
  ]

mkVarDecl :: Show e => Type -> [(Name,Maybe e)] -> Statement e -> Statement e
mkVarDecl typ vars stmt = foldr (uncurry (flip VarDecl typ)) stmt vars
pVarInitializer = token Assignment *> pExpression
pElse = keyword "else" *> pStatement

pExpression = pLeftExpression <**> pExpressionSuffix

pLeftExpression = choice
  [inParens pExpression
  ,InF . EVarRef <$> pName
  ,InF . EString . snd <$> string -- FIXME Position is thrown away
  ,InF . EInt . snd <$> integer -- FIXME Position is thrown away
  ,InF (EBool True) <$ keyword "true"
  ,InF (EBool False) <$ keyword "false"
  ]

pAssignmentOperator = token Assignment -- TODO Also handle operator-assignments, once lexer and token definitions have it.

eAssignment a b c = InF (EAssignment a b c)
eBinop a b c = InF (EBinary a b c)

pExpressionSuffix :: Parser Token (ExprF -> ExprF)
pExpressionSuffix = choice
  [flip <$> (eAssignment <$> pAssignmentOperator) <*> pExpression
  ,flip <$> (eBinop <$> pBinop) <*> pExpression
  ,(InF .) <$> flip EFunCall <$> inParens (listOf pExpression)
  ,pure id
  ]

pBinop = choice
  [token Equal]

keyword str = token (Reserved str)
parseJust f = second (fromJust . f) <$> satisfy (isJust . f . snd)
identifier = parseJust fromIdentifier
integer = second fromIntegral <$> parseJust fromIntegerTok
string = parseJust fromStringTok

fromIdentifier (Identifier s) = Just s
fromIdentifier _ = Nothing

fromIntegerTok (IntegerTok i) = Just i
fromIntegerTok _ = Nothing

fromStringTok (StringTok s) = Just s
fromStringTok _ = Nothing

inBraces p = token OpenBrace *> p <* token CloseBrace
inBrackets p = token OpenBracket *> p <* token CloseBracket
inParens p = token OpenParen *> p <* token CloseParen

listOf p = sepBy p (token Comma)

