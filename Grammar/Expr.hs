{-# LANGUAGE NoMonomorphismRestriction #-}

module Grammar.Expr (pExpression, pExpressionList, pInitializationExpression) where

import Control.Applicative

import Data.Maybe

import AST
import CppToken

import Grammar.Parser
import Grammar.Types
import Grammar.Utils

{-eSeq a b = InF (ESeq a b)
eUnary (_,Asterix) b = InF (EDeref b)
eUnary a b = InF (EUnary a b)
eAssignment a b c = InF (EAssignment a b c)
eConditional a b c = InF (EConditional a b c)
eBinop a b c = InF (EBinary a b c)
eCast typ e = InF (ECast typ e)-}

unLocE (LocE _ e) = e
locE p = (\(Loc l e) -> LocE l e) <$> addLocation p
(<**?>) :: MParser LocE -> MParser (LocE -> Expr LocE) -> MParser LocE
p <**?> maybefp = locE (p <**> (fromMaybe unLocE <$> optional maybefp))

pExpressionList :: MParser [LocE]
pExpressionList = listOf pAssignmentExpression

pExpression :: MParser LocE
pExpression = pAssignmentExpression <**?> (token Comma *> (flip ESeq <$> pExpression))

-- pExpression without (top-level) assignmnt or sequence expressions
pInitializationExpression :: MParser LocE
pInitializationExpression = pLogicalOrExpression <**?> pConditionalSuffix

pAssignmentExpression = pLogicalOrExpression <**?> choice [pConditionalSuffix,suffix]
  where
    suffix = flip <$> (EAssignment <$> pAssignmentOperator) <*> pAssignmentExpression

-- TODO Generalize these into:
--  * left-hand ("lower") expression
--  * list of operators that can follow
--  * function to apply to said list

pConditionalSuffix = token QuestionMark *!> ((\t f cond -> EConditional cond t f) <$> pExpression <* token SingleColon <*> pAssignmentExpression)

pLogicalOrExpression = {- skip a few logical operators -} pEqualityExpression

pEqualityExpression = pRelationalExpression <**?> suffix
  where
    -- FIXME Does this give the right grouping?
    suffix = flip <$> (EBinary <$> (token Equal <|> token NotEqual)) <*!> pEqualityExpression

pRelationalExpression = pShiftExpression <**?> choice suffices
  where
    suffices = map suffix [LessThan,GreaterThan,LessOrEqual,GreaterOrEqual]
    -- FIXME This gives the wrong grouping - a<(b<c) instead of (a<b)<c
    -- How fix?
    suffix tok = flip <$> (EBinary <$> token tok) <*!> pRelationalExpression

pShiftExpression = pAdditiveExpression -- TODO bitshifts
pAdditiveExpression = pMultiplicativeExpression <**?> choice suffices
  where
    suffices = map suffix [Minus,Plus] ++ [failParse "Expected '+' or '.'"]
    suffix tok = flip <$> (EBinary <$> token tok) <*!> pAdditiveExpression

pMultiplicativeExpression = pCastExpression
pCastExpression = pUnaryExpression
pUnaryExpression :: MParser LocE
pUnaryExpression = choice
  [error "prefix increment, unimpl" <$ token Increment *> pCastExpression
  ,error "prefix decrement, unimpl" <$ token Decrement *> pCastExpression
  ,locE (pUnaryOperator <*> pCastExpression)
  ,pPostfixExpression
  ] -- should probably include sizeof, the internal support for it is likely to be required anyway
  <|> failParse "pUnaryExpression"

pPostfixExpression :: MParser LocE
pPostfixExpression = pPrimaryExpression <**?> choice
  [EPostfix <$> postfixOperator
  ,flip EFunCall <$> inParens pExpressionList
  ,flip EArrayIndex <$> inBrackets pExpression
  -- ,(InF .) <$> ETypeApp <$> inTypeBrackets (listOf pType)
  ,EFieldAccess <$> (token Dot *> pName)
  ]

pPrimaryExpression = inParens pExpression <|>
  locE (choice
  [ENullPtr <$ keyword "null"
  ,EVarRef <$> pName
  ,EString . snd <$> string -- FIXME Position is thrown away
  ,EInt . snd <$> integer -- FIXME Position is thrown away
  ,EChar . snd <$> char -- FIXME Position is thrown away
  ,EBool True <$ keyword "true"
  ,EBool False <$ keyword "false"
  ,keyword "cast" *!> (ECast <$> inTypeBrackets pType <*> inParens pExpression)
  ] <|> failParse "Out of luck in pLeftExpression")

pAssignmentOperator = choice . map token $
  [Assignment
  ,PlusAssign
  ] -- TODO Also handle operator-assignments, once lexer and token definitions have it.

pUnaryOperator :: MParser (LocE -> Expr LocE)
pUnaryOperator = eUnary <$> (choice (map token unaryOperators) <|> failParse "Expected unary operator")
  where
    eUnary (_,Asterix) = EDeref
    eUnary op = EUnary op
