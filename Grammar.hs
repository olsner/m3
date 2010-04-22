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

pUnit = Unit <$> many pImport <*> (pModule <|> head <$> pFunction) <* eof

pModule = keyword "module" *> (Decl <$> pName) <* token Semicolon <*> (ModuleDef . concat <$> commit (many pTopLevelDecl))
pTopLevelDecl :: Parser Token [Decl ExprF]
pTopLevelDecl = pExternalFunction <|> pFunction <|> (pVarDecl (\typ name e -> (Decl name (VarDef typ e):)) <*> pure [])

pImport :: Parser Token Name
pImport = keyword "import" *> pName <* token Semicolon
pSimpleName = (\(_,nm) -> QualifiedName [nm]) <$> identifier
pName = QualifiedName . map snd <$> sepBy1 identifier (token DoubleColon)

pFunction = (\ret nm params code -> [Decl nm (FunctionDef ret params code)]) <$>
    pType <*> pName <*> pFormalParamList <*> commit pCompoundStatement
pExternalFunction = keyword "extern" *> commit (
    (\linkage ret nm params -> [Decl nm (ExternalFunction (fmap snd linkage) ret params)])
      <$> optional string <*> pType <*> pName <*> pFormalParamList
      <* token Semicolon)

pFormalParamList = inParens (listOf $ choice [pFormalParam, pVarargParam])
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
pStatement = choice
  [token Semicolon $> EmptyStmt
  ,ReturnStmtVoid <$ (keyword "return" *> token Semicolon)
  ,ReturnStmt <$> (keyword "return" *> pExpression <* token Semicolon)
  ,ExprStmt <$> pExpression <* token Semicolon
  -- Note: This syntax defines the scope of a variable as "everything until the next closing brace. I this (really) correct? No it isn't...
  -- TODO: What happens with e.g. if (foo) type var; !?
  ,pVarDecl (flip VarDecl) <*> commit (CompoundStmt <$> many pStatement <* lookToken CloseBrace)
  ,CompoundStmt <$> pCompoundStatement
  ,keyword "if" *> commit (IfStmt <$> inParens pExpression <*> pStatement <*> (fromMaybe EmptyStmt <$> optional pElse))
  ,keyword "while" *> commit (WhileStmt <$> inParens pExpression <*> pStatement)
  ] <|> failParse "Out of luck in pStatement"

pVarDecl :: (Type -> Name -> Maybe ExprF -> a -> a) -> Parser Token (a -> a)
pVarDecl f = mkVarDecl f <$> pType <*> sepBy1 ((,) <$> pName <*> optional pVarInitializer) (token Comma) <* token Semicolon
mkVarDecl :: Show e => (Type -> Name -> Maybe e -> a -> a) -> Type -> [(Name,Maybe e)] -> a -> a
mkVarDecl varDecl typ vars z = foldr (uncurry (varDecl typ)) z vars
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

