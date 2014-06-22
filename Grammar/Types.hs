{-# LANGUAGE NoMonomorphismRestriction #-}

module Grammar.Types (pType, genVarDecl, pTypedef) where

import Control.Applicative

import AST
import CppToken

import Grammar.Parser
import Grammar.Utils

typeIdentifier :: MParser (Pos,Type)
typeIdentifier = satisfyLookState "type identifier" f <* next
  where
    -- TODO should use a parser-for-names instead.
    f s (Identifier id) = maybe empty pure (lookupTypeIdentifier s (QualifiedName [id]))
    f _ _ = empty

pStructField = genVarDecl (\typ name () -> (name, typ)) (pure ())

pType = pLeftType <**> pArraySuffix
pLeftType = choice
  [keyword "void" $> TVoid
  ,keyword "int" $> TInt
  ,keyword "char" $> TChar
  ,keyword "bool" $> TBool
  ,keyword "const" *> (TConst <$> pType)
  ,keyword "struct" *> (TStruct <$> inBraces (concat <$> many pStructField))
  ,inBrackets (TPtr <$> commit pType)
  -- FIXME snd <$> due to not tracking positions of types directly
  ,snd <$> typeIdentifier
  ] <|> failParse "Out of luck in pLeftType"
pArraySuffix = maybe id TArray <$> optional (snd <$> inBrackets integer)

pTypedef f = choice
  [keyword "type" *!> ((,) <$> pName <*> (token Assignment *> pType)) <*! token Semicolon
  ] >>= \(name,typ) -> stateAddType name typ >> return (f name typ)

genVarDecl :: (Type -> Name -> b -> a) -> MParser b -> MParser [Loc a]
genVarDecl f init = mkVarDecl f <$> pType <*> sepBy1 (addLocation ((,) <$> pName <*> init)) (token Comma) <* token Semicolon
mkVarDecl :: (Type -> Name -> b -> a) -> Type -> [Loc (Name,b)] -> [Loc a]
mkVarDecl varDecl typ vars = map (\(Loc l (n,b)) -> Loc l (varDecl typ n b)) vars

