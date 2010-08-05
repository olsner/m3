{-# LANGUAGE NoMonomorphismRestriction #-}

module Grammar.Types (pType) where

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

