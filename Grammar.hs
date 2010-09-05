{-# LANGUAGE NoMonomorphismRestriction #-}

module Grammar
  (pUnit
  ,pName
  ,runParser {- from Parser -}
  ,initialParserState {- from Parser.Utils -}
  ) where

import Control.Applicative

import AST
import CppToken

import Grammar.Expr
import Grammar.Parser
import Grammar.Types
import Grammar.Utils

singleton x = [x]

pUnit = Unit <$> many pImport <*> addLocation (pModule <|> pFunction <* eof)

pModule = keyword "module" *> (Decl <$> pName) <* token Semicolon <*> (ModuleDef . concat <$> commit (many pTopLevelDecl))
pTopLevelDecl :: MParser [LocDecl LocE]
pTopLevelDecl = singleton <$> addLocation (choice
  [pExternalFunction
  ,pFunction
  ,pTypedef (\name typ -> Decl name (TypeDef typ))
  ]) <|>
    pVarDecl (\typ name e -> Decl name (VarDef typ e))

pImport :: MParser Name
pImport = keyword "import" *> pName <* token Semicolon

pFunction = (\ret nm params code -> Decl nm (FunctionDef ret params code)) <$>
    pType <*> pName <*> pFormalParamList <*> commit (addLocation pCompoundStatement)
pExternalFunction = keyword "extern" *> commit (
    (\linkage ret nm params -> Decl nm (ExternalFunction (fmap snd linkage) ret params))
      <$> optional string <*> pType <*> pName <*> pFormalParamList
      <* token Semicolon)

pFormalParamList = inParens (listOf $ choice [pFormalParam, pVarargParam])

pFormalParam = FormalParam <$> pType <*> optional pSimpleName
pVarargParam = VarargParam <$ token Ellipsis

pCompoundStatement = CompoundStmt [] <$> inBraces (commit (many pStatement))
pStatement = addLocation $ choice
  [token Semicolon $> EmptyStmt
  ,pTypedef TypDecl
  ,VarDecl <$> pVarDecl (,,)
  ,ReturnStmt <$> (keyword "return" *> pExpression <* commit (token Semicolon))
  ,ReturnStmtVoid <$ (keyword "return" *> commit (token Semicolon))
  ,ExprStmt <$> pExpression <* commit (token Semicolon)
  ,pCompoundStatement
  ,keyword "if" *> commit (IfStmt <$> inParens pExpression <*> pStatement <*> pElse)
  ,keyword "while" *> commit (WhileStmt <$> inParens pExpression <*> pStatement)
  ] <|> failParse "Out of luck in pStatement"

pVarDecl f = genVarDecl f (optional pVarInitializer)

pVarInitializer = token Assignment *> pInitializationExpression
pElse = (keyword "else" *> pStatement) <|> addLocation (pure EmptyStmt)

