{-# LANGUAGE NoMonomorphismRestriction #-}

module Grammar.Types (pType, genVarDecl, pTypedef) where

import Control.Applicative

import AST
import CppToken

import Grammar.Parser
import Grammar.Utils

typeIdentifier = satisfyLookState "type identifier" f <* next
  where
    -- TODO should use a parser-for-names instead.
    f s (pos,Identifier id) = maybe empty pure (lookupTypeIdentifier s (QualifiedName [id]))
    f s _ = empty

pType = pLeftType <**> pArraySuffix
pLeftType = choice
  [keyword "void" $> TVoid
  ,keyword "int" $> TInt
  ,keyword "char" $> TChar
  ,keyword "bool" $> TBool
  ,keyword "const" *> (TConst <$> pType)
  ,inBrackets (commit (TPtr <$> pType))
  ,typeIdentifier
  ] <|> failParse "Out of luck in pLeftType"
pArraySuffix = maybe id TArray <$> optional (snd <$> inBrackets integer)

pTypedef = choice
  [keyword "type" *> ((,) <$> pName <*> (token Assignment *> pType))
  ] >>= \(name,typ) -> stateAddType name typ >> return (Decl name (TypeDef typ))

genVarDecl :: (Type -> Name -> b -> a) -> MParser b -> MParser [a]
genVarDecl f init = mkVarDecl f <$> pType <*> sepBy1 ((,) <$> pName <*> init) (token Comma) <* token Semicolon
mkVarDecl :: (Type -> Name -> b -> a) -> Type -> [(Name,b)] -> [a]
mkVarDecl varDecl typ vars = map (uncurry (varDecl typ)) vars

