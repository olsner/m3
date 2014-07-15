{-# LANGUAGE NoMonomorphismRestriction #-}

module Grammar.Types (pType, genVarDecl, pTypedef) where

import Control.Applicative

import AST
import CppToken

import Grammar.Parser
import Grammar.Utils

pStructField = genVarDecl (\typ name () -> (name, typ)) (pure ())

pType = (wrapT <$> pLeftType) <**> pArraySuffix
pLeftType = choice
  [keyword "void" $> TVoid
  ,keyword "int" $> TInt
  ,keyword "char" $> TChar
  ,keyword "bool" $> TBool
  ,keyword "const" *> (TConst <$> pType)
  ,keyword "struct" *> (TStruct <$> inBraces (concat <$> many pStructField))
  --,keyword "let" $> TFreshVariable
  ,inBrackets (TPtr <$> commit pType)
  ,TNamedType <$> pName
  ] <|> failParse "Out of luck in pLeftType"
--pArraySuffix :: MParser (FType -> FType)
pArraySuffix = maybe id (\n -> wrapT . TArray n) <$> optional (snd <$> inBrackets integer)

pTypedef f = choice
  [keyword "type" *!> (f <$> pName <*> (token Assignment *> pType)) <*! token Semicolon
  ]

genVarDecl :: TypeF f => (f -> Name -> b -> a) -> MParser b -> MParser [Loc a]
genVarDecl f init = mkVarDecl f <$> pType <*> sepBy1 (addLocation ((,) <$> pName <*> init)) (token Comma) <* token Semicolon
mkVarDecl :: TypeF f => (f -> Name -> b -> a) -> f -> [Loc (Name,b)] -> [Loc a]
mkVarDecl varDecl typ vars = map (\(Loc l (n,b)) -> Loc l (varDecl typ n b)) vars

