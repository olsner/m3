{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Applicative
import Control.Arrow
import Control.Functor.Fix
import Control.Monad.Reader
import Control.Monad.State

import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as M

import System.Directory
import System.FilePath
import System.Exit

import Text.ParserCombinators.Parsec.Pos
import Text.Printf

import AST
import CppToken
import CppLexer (lexCpp)
import Parser
import CodeGen
import TypeCheck (typecheck)

none p = not . any p
token t = satisfy ((== t) . snd)
lookToken t = satisfyLook ((== t) . snd)

pUnit = Unit <$> many pImport <*> (pModule <|> pFunction)

pModule = keyword "module" >> (Decl <$> pName) <* token Semicolon <*> (ModuleDef <$> many pFunction) <* eof

pImport = keyword "import" >> pName <* token Semicolon
pSimpleName = (\(pos,nm) -> QualifiedName [nm]) <$> identifier
pName = QualifiedName . map snd <$> sepBy1 identifier (token DoubleColon)

pFunction = choice
  [(\ret nm params code -> Decl nm (FunctionDef ret params code)) <$>
    pType <*> pName <*> pFormalParamList <*> pCompoundStatement
  ,keyword "extern" *> (
    (\linkage ret nm params -> Decl nm (ExternalFunction (fmap snd linkage) ret params))
      <$> optional string <*> pType <*> pName <*> pFormalParamList
      <* token Semicolon)
  ]

pFormalParamList = do
  xs <- inParens . listOf . choice $ [pFormalParam, pVarargParam]
  guardMsg (validateFormalParams xs) "Invalid formal parameter list - vararg ellipsis must be last parameter."
  return xs
pCompoundStatement = inBraces (many pStatement)

pFormalParam = FormalParam <$> pType <*> optional pSimpleName
pVarargParam = token Ellipsis >> return VarargParam
validateFormalParams (x:xs) = none (== VarargParam) (init xs)
validateFormalParams [] = True

infixl 3 $>
($>) = flip (<$)

pType = pArraySuffix =<< choice
  [keyword "void" $> TVoid
  ,keyword "int" $> TInt
  ,keyword "char" $> TChar
  ,keyword "const" >> (TConst <$> pType)
  ,inBrackets (TPtr <$> pType)
  ]
pArraySuffix t = optional (snd <$> inBrackets integer) >>= maybe (return t) (pArraySuffix . flip TArray t)

pStatement = choice $
  [token Semicolon $> EmptyStmt
  ,keyword "return" >> token Semicolon >> return ReturnStmtVoid
  ,keyword "return" *> (ReturnStmt <$> pExpression) <* token Semicolon
  ,ExprStmt <$> pExpression >>= \t -> t `seq` token Semicolon >> return t
  -- TODO Implement declarations of multiple variables in one statement
  ,flip VarDecl <$> pType <*> pName <*> (token Semicolon >> CompoundStmt <$> many pStatement <* lookToken CloseBrace)
  ,CompoundStmt <$> inBraces (many pStatement)
  ]

pExpression = pLeftExpression <**> pExpressionSuffix

pLeftExpression = choice
  [inParens pExpression
  ,InF . EVarRef <$> pName
  ,InF . EString . snd <$> string -- FIXME Position is thrown away
  ,InF . EInt . snd <$> integer -- FIXME Position is thrown away
  ]

pAssignmentOperator = token Assignment -- TODO Also handle operator-assignments, once lexer and token definitions have it.

eAssignment a b c = InF (EAssignment a b c)

pExpressionSuffix :: Parser Token (ExprF -> ExprF)
pExpressionSuffix = choice
  [flip <$> (eAssignment <$> pAssignmentOperator) <*> pExpression
  ,(InF .) <$> flip EFunCall <$> inParens (listOf pExpression)
  ,return id
  ]

keyword str = token (Identifier str) <|> token (Reserved str)
parseJust f = second (fromJust . f) <$> satisfy (isJust . f . snd)
identifier = parseJust fromIdentifier
integer = second fromIntegral <$> parseJust fromIntegerTok
string = parseJust fromStringTok

fromIdentifier (Identifier s) = Just s
fromIdentifier (Reserved s) = Just s
fromIdentifier _ = Nothing

fromIntegerTok (IntegerTok i) = Just i
fromIntegerTok _ = Nothing

fromStringTok (StringTok s) = Just s
fromStringTok _ = Nothing

inBraces p = token OpenBrace *> p <* token CloseBrace
inBrackets p = token OpenBracket *> p <* token CloseBracket
inParens p = token OpenParen *> p <* token CloseParen

listOf p = sepBy p (token Comma)

parse path = do
  input <- readFile path
  let res = lexCpp path input
  --print res
  case res of
    Left err -> do
      putStrLn "Error in lexical analysis:" >> print err
      exitFailure
    Right tokens -> do
      --mapM_ print (map snd tokens)
      return (fst $ runParser pUnit tokens)

process :: Name -> ModMap -> IO ()
process name mods = do
  let Just ast = M.lookup name mods
  mods' <- runReaderT (typecheck name) mods
  runReaderT (printLLVM name) mods'

firstM :: Monad m => (a -> m (Maybe b)) -> [a] -> m b
firstM f (x:xs) = do
  fx <- f x
  case fx of
    Just x -> return x
    Nothing -> firstM f xs
firstM f [] = fail "Failed"

nameToPath (QualifiedName components) = joinPath components

tryImportModule name path = do
  let modPath = addExtension (path </> nameToPath name) ".m"
  printf "tryImportModule: %s: Trying %s\n" (show name) modPath
  e <- doesFileExist modPath
  if e then Just <$> parse modPath else return Nothing

includePath = ["stdlib", "tests"]

type ModMap = Map Name (Unit ExprF)
type ModT = StateT ModMap
type Mod = ModT IO

runMod = flip runStateT

ifNotLoaded name m = gets (M.lookup name) >>= \res -> case res of
  Just mod -> return mod
  Nothing -> m >>= \mod -> mod <$ modify (M.insert name mod)

processImport :: Name -> Mod (Unit ExprF)
processImport name = ifNotLoaded name $ do
  unit <- liftIO $ firstM (tryImportModule name) includePath
  mapM_ processImport (unitImports unit)
  return unit

-- TODO for each cmd-line arg, parse as ::-separated name and compile
main = doMain (QualifiedName ["ex3"])

doMain name = do
  (mod,mods) <- runMod M.empty (processImport name)
  process name mods
