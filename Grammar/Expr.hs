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

leftAssoc :: MParser a -> MParser b -> (a -> b -> a -> a) -> MParser a
leftAssoc lower op fun = lower <**> rest
  where
    rest = fromMaybe id <$> optional tail
    tail = (\op mid rest left -> rest (fun left op mid)) <$> op <*!> lower <*!> rest

leftAssocBinops lower tokens = leftAssoc lower ops fun
  where
    ops = choice $ map (addLocation . token) tokens ++ [failParse ("Expected one of "++show tokens)]
    fun left (Loc l t) right = LocE l (EBinary t left right)

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

pConditionalSuffix = token QuestionMark *!> ((\t f cond -> EConditional cond t f) <$> pExpression <* token SingleColon <*> pAssignmentExpression)

pLogicalOrExpression = {- skip a few logical operators -} pEqualityExpression

pEqualityExpression = leftAssocBinops pRelationalExpression $
  [Equal, NotEqual]

pRelationalExpression = leftAssocBinops pShiftExpression $
  [LessThan,GreaterThan,LessOrEqual,GreaterOrEqual]

pShiftExpression = pAdditiveExpression -- TODO bitshifts

pAdditiveExpression = leftAssocBinops pMultiplicativeExpression $
  [Minus,Plus]
pMultiplicativeExpression = leftAssocBinops pCastExpression $
  [Asterix,Division,Modulo]

pCastExpression = pUnaryExpression
pUnaryExpression :: MParser LocE
pUnaryExpression = choice
  [locE (EPrefix <$> prefixOperator <*> pCastExpression)
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
pUnaryOperator = eUnary <$> choice (map token unaryOperators)
  where
    eUnary (_,Asterix) = EDeref
    eUnary op = EUnary op
