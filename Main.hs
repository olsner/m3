import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as M

import System.Directory
import System.FilePath
import System.Environment
import System.Exit

import Text.Printf

import AST
import CppLexer (lexCpp)
import Parser
import CodeGen
import TypeCheck (typecheck)
import ScopeCheck (scopecheck)
import Grammar (pUnit, pName)

parse path = do
  input <- readFile path
  let res = lexCpp path input
  case res of
    Left err -> do
      putStrLn "Error in lexical analysis:" >> print err
      exitFailure
    Right tokens ->
      case runParser pUnit tokens of
        (Right res, []) -> return res
        (res, rest) -> do
          putStrLn "*** Parse left residue (only 10 tokens shown):"
          mapM_ print (take 10 rest)
          --putStrLn "*** Full token stream:"
          --mapM_ print (map snd tokens)
          let msg = case res of Left err -> err; _ -> show res
          putStrLn ("Parse error "++msg)
          exitFailure

firstM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
firstM f (x:xs) = do
  fx <- f x
  maybe (firstM f xs) (return . Just) fx
firstM _ [] = return Nothing

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

runMod = flip execStateT

ifNotLoaded name m = gets (M.lookup name) >>= \res -> case res of
  Just modul -> return modul
  Nothing -> m >>= \modul -> modul <$ modify (M.insert name modul)

processImport :: Name -> Mod (Unit ExprF)
processImport name = ifNotLoaded name $ do
  res <- liftIO $ firstM (tryImportModule name) includePath
  case res of
    Just unit -> do
      mapM_ processImport (unitImports unit)
      return unit
    Nothing -> error ("Error: Can't locate module "++show name)

parseName name = case lexCpp "cmd-line" name of
  Left err -> putStrLn "Error: Can't parse name:" >> print err >> exitFailure
  Right tokens -> case fst (runParser pName tokens) of
    Left err -> putStrLn "Error: Can't parse name:" >> print err >> exitFailure
    Right name -> return name
 

main = mapM_ (doMain <=< parseName) =<< getArgs

doMain name = do
  mods <- runMod M.empty (processImport name)
  process name mods

mapMapM f m = M.fromList <$> mapM (secondM f) (M.toList m)
  where
    secondM f (a,b) = f b >>= \b' -> return (a,b')

process :: Name -> ModMap -> IO ()
process name mods'' = do
  mods' <- mapMapM scopecheck mods''
  mods <- runReaderT (typecheck name) mods'
  mapM_ print (M.toList mods)
  runReaderT (printLLVM name) mods
