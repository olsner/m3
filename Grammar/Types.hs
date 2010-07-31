{-# LANGUAGE NoMonomorphismRestriction #-}

module Grammar.Types (pType) where

import Control.Applicative

import AST

import Grammar.Parser
import Grammar.Utils

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

