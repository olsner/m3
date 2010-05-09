{-# LANGUAGE NoMonomorphismRestriction #-}

module Grammar (pUnit, pName) where

import Control.Applicative

import Data.Maybe

import AST
import CppToken
import Parser

import Grammar.Expr
import Grammar.Utils

pUnit = Unit <$> many pImport <*> (pModule <|> head <$> pFunction) <* eof

pModule = keyword "module" *> (Decl <$> pName) <* token Semicolon <*> (ModuleDef . concat <$> commit (many pTopLevelDecl))
pTopLevelDecl :: Parser Token [Decl ExprF]
pTopLevelDecl = pExternalFunction <|> pFunction <|> (pVarDecl (\typ name e -> (Decl name (VarDef typ e):)) <*> pure [])

pImport :: Parser Token Name
pImport = keyword "import" *> pName <* token Semicolon

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
  ,keyword "bool" $> TBool
  ,keyword "const" *> (TConst <$> pType)
  ,inBrackets (commit (TPtr <$> pType))
  ] <|> failParse "Out of luck in pLeftType"
pArraySuffix = maybe id TArray <$> optional (snd <$> inBrackets integer)

pCompoundStatement = inBraces (commit (many pStatement))
pStatement = choice
  [token Semicolon $> EmptyStmt
  ,ReturnStmt <$> (keyword "return" *> pExpression <* commit (token Semicolon))
  ,ReturnStmtVoid <$ (keyword "return" *> commit (token Semicolon))
  ,ExprStmt <$> pExpression <* commit (token Semicolon)
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

