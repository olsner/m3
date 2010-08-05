{-# LANGUAGE NoMonomorphismRestriction #-}

module Grammar
  (pUnit
  ,pName
  ,runParser {- from Parser -}
  ,initialParserState {- from Parser.Utils -}
  ) where

import Control.Applicative

import Data.Maybe

import AST
import CppToken

import Grammar.Expr
import Grammar.Parser
import Grammar.Types
import Grammar.Utils

pUnit = Unit <$> many pImport <*> (pModule <|> head <$> pFunction) <* eof

pModule = keyword "module" *> (Decl <$> pName) <* token Semicolon <*> (ModuleDef . concat <$> commit (many pTopLevelDecl))
pTopLevelDecl :: MParser [Decl ExprF]
pTopLevelDecl = pExternalFunction <|> pFunction <|> (pVarDecl (\typ name e -> Decl name (VarDef typ e)))

pImport :: MParser Name
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

pCompoundStatement = CompoundStmt [] <$> inBraces (commit (many pStatement))
pStatement = choice
  [token Semicolon $> EmptyStmt
  ,ReturnStmt <$> (keyword "return" *> pExpression <* commit (token Semicolon))
  ,ReturnStmtVoid <$ (keyword "return" *> commit (token Semicolon))
  ,ExprStmt <$> pExpression <* commit (token Semicolon)
  -- Note: This syntax defines the scope of a variable as "everything until the next closing brace. I this (really) correct? No it isn't...
  -- TODO: What happens with e.g. if (foo) type var; !?
  ,VarDecl <$> pVarDecl (,,)
  ,pCompoundStatement
  ,keyword "if" *> commit (IfStmt <$> inParens pExpression <*> pStatement <*> (fromMaybe EmptyStmt <$> optional pElse))
  ,keyword "while" *> commit (WhileStmt <$> inParens pExpression <*> pStatement)
  ] <|> failParse "Out of luck in pStatement"

pVarDecl :: (Type -> Name -> Maybe ExprF -> a) -> MParser [a]
pVarDecl f = mkVarDecl f <$> pType <*> sepBy1 ((,) <$> pName <*> optional pVarInitializer) (token Comma) <* token Semicolon
mkVarDecl :: Show e => (Type -> Name -> Maybe e -> a) -> Type -> [(Name,Maybe e)] -> [a]
mkVarDecl varDecl typ vars = map (uncurry (varDecl typ)) vars
pVarInitializer = token Assignment *> pInitializationExpression
pElse = keyword "else" *> pStatement

