{-# LANGUAGE NoMonomorphismRestriction #-}

module Grammar.Expr (pExpression, pExpressionList) where

import Control.Applicative
import Control.Functor.Fix

import Data.Maybe

import Debug.Trace

import AST
import CppToken
import Parser

import Grammar.Utils

eSeq a b = InF (ESeq a b)
eUnary (_,Asterix) b = InF (EDeref b)
eUnary a b = InF (EUnary a b)
eAssignment a b c = InF (EAssignment a b c)
eConditional a b c = InF (EConditional a b c)
eBinop a b c = InF (EBinary a b c)

(<**?>) :: Alternative f => f a -> f (a -> a) -> f a
p <**?> maybefp = p <**> (fromMaybe id <$> optional maybefp)

pExpressionList = listOf pAssignmentExpression

pExpression :: Parser Token ExprF
pExpression = pAssignmentExpression <**?> (token Comma *> (flip eSeq <$> pExpression))

pAssignmentExpression = pLogicalOrExpression <**?> choice [pConditionalSuffix,suffix]
  where
    suffix = flip <$> (eAssignment <$> pAssignmentOperator) <*> pAssignmentExpression

-- TODO Generalize these into:
--  * left-hand ("lower") expression
--  * list of operators that can follow
--  * function to apply to said list

pConditionalExpression = pLogicalOrExpression <**?> pConditionalSuffix
pConditionalSuffix = token QuestionMark *> commit ((\t f cond -> eConditional cond t f) <$> pExpression <* token SingleColon <*> pAssignmentExpression)

pLogicalOrExpression = {- skip a few logical operators -} pEqualityExpression

pEqualityExpression = pRelationalExpression <**?> suffix
  where
    -- FIXME Does this give the right grouping?
    suffix = flip <$> (eBinop <$> (token Equal <|> token NotEqual)) <*> commit pEqualityExpression

pRelationalExpression = pShiftExpression <**?> choice suffices
  where
    suffices = map suffix [LessThan,GreaterThan,LessOrEqual,GreaterOrEqual]
    -- FIXME This gives the wrong grouping - a<(b<c) instead of (a<b)<c
    -- How fix?
    suffix tok = flip <$> (eBinop <$> token tok) <*> commit pRelationalExpression

pShiftExpression = pAdditiveExpression -- TODO bitshifts
pAdditiveExpression = pMultiplicativeExpression <**?> choice suffices
  where
    suffices = map suffix [Minus,Plus] ++ [failParse "Expected '+' or '.'"]
    suffix tok = flip <$> (eBinop <$> token tok) <*> commit pAdditiveExpression

pMultiplicativeExpression = pCastExpression
pCastExpression = pUnaryExpression
pUnaryExpression :: Parser Token ExprF
pUnaryExpression = choice
  [error "prefix increment, unimpl" <$ token Increment *> pCastExpression
  ,error "prefix decrement, unimpl" <$ token Decrement *> pCastExpression
  ,pUnaryOperator <*> pCastExpression
  ,pPostfixExpression
  ] -- should probably include sizeof, the internal support for it is likely to be required anyway
  <|> failParse "pUnaryExpression"

pPostfixExpression :: Parser Token ExprF
pPostfixExpression = pPrimaryExpression <**?> choice
  [(InF .) <$> (EPostfix <$> postfixOperator)
  ,(InF .) <$> flip EFunCall <$> inParens pExpressionList
  ,(InF .) <$> flip EArrayIndex <$> inBrackets pExpression
  -- ,(InF .) <$> ETypeApp <$> inTypeBrackets (listOf pType)
  ]

pPrimaryExpression = choice
  [inParens pExpression
  ,InF ENullPtr <$ keyword "null"
  ,InF . EVarRef <$> pName
  ,InF . EString . snd <$> string -- FIXME Position is thrown away
  ,InF . EInt . snd <$> integer -- FIXME Position is thrown away
  ,InF . EChar . snd <$> char -- FIXME Position is thrown away
  ,InF (EBool True) <$ keyword "true"
  ,InF (EBool False) <$ keyword "false"
  -- TODO Move to right place
  --,InF . EDeref <$> (token Asterix *> pLeftExpression)
  ] <|> failParse "Out of luck in pLeftExpression"

pAssignmentOperator = choice . map token $
  [Assignment
  ,PlusAssign
  ] -- TODO Also handle operator-assignments, once lexer and token definitions have it.

pUnaryOperator :: Parser Token (ExprF -> ExprF)
pUnaryOperator = eUnary <$> (choice (map token unaryOperators) <|> failParse "Expected unary operator")

