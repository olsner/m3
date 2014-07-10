import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State

import Data.List
import Data.Map (Map)
import qualified Data.Map as M

import System.Directory
import System.FilePath
import System.Environment
import System.Exit
import System.IO

import Text.Printf

import AST
import CppLexer (lexCpp)
import CodeGen
import TypeCheck (typecheck)
import ScopeCheck (scopecheck)
import Grammar (pUnit, pName, runParser, initialParserState)

printError :: Location -> String -> a
printError loc msg = error (show loc ++ ": " ++ msg)

cmdError :: String -> a
cmdError = error

parse path = do
  input <- readFile path
  -- We use only the filename so that tests can run in a temporary directory
  -- but still produce predictable error messages.
  let res = lexCpp (takeFileName path) input
  case res of
    Left err -> do
      hPutStrLn stderr "Error in lexical analysis:" >> print err
      exitFailure
    Right tokens ->
      case runParser pUnit initialParserState tokens of
        (Right (res,_),_) -> return res
        (res,_rest) -> do
          -- TODO Flags for debug output...
          --putStrLn "*** Parse left residue (only 10 tokens shown):"
          --mapM_ print (take 10 rest)
          --putStrLn "*** Full token stream:"
          --mapM_ print (map snd tokens)
          let msg = case res of Left err -> err; _ -> show res
          hPutStrLn stderr msg
          exitFailure

firstM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
firstM f (x:xs) = do
  fx <- f x
  maybe (firstM f xs) (return . Just) fx
firstM _ [] = return Nothing

nameToPath (QualifiedName components) = joinPath components

tryImportModule (Loc _ name) path = do
  let modPath = addExtension (path </> nameToPath name) ".m"
  --printf "tryImportModule: %s: Trying %s\n" (show name) modPath
  e <- doesFileExist modPath
  if e then Just <$> parse modPath else return Nothing

defaultIncludePath = ["stdlib", "tests"]

type ModMap = Map Name (Maybe (Unit LocE))
type ModT m = ReaderT Options (StateT ModMap m)
type Mod = ModT IO

runMod :: Options -> ModMap -> Mod a -> IO ModMap
runMod opts mods m = execStateT (runReaderT m opts) mods

ifNotLoaded (Loc loc name) m = do
  res <- gets (M.lookup name)
  case res of
    Just Nothing -> printError loc ("Error: Recursive import of " ++ show name)
    Just mod -> return mod
    Nothing -> do
      modify (M.insert name Nothing)
      res <- m
      case res of
        Just mod -> Just mod <$ modify (M.insert name (Just mod))
        Nothing -> return Nothing

importName name = ifNotLoaded name $ do
  inc <- asks includePath
  liftIO $ firstM (tryImportModule name) inc

processUnit unit = do
  mapM_ processImport (unitImports unit)
  return unit

processImport :: Loc Name -> Mod (Unit LocE)
processImport lname@(Loc loc name) = do
  res <- importName lname
  case res of
    Just unit -> processUnit unit
    Nothing -> printError loc ("Error: Can't locate module "++show name)

parseName name = case lexCpp "cmd-line" name of
  Left err -> cmdError ("Error: Can't lex name: " ++ show err)
  Right tokens -> case fst (runParser pName () tokens) of
    Left err -> cmdError ("Error: Can't parse name: " ++ show err)
    Right (name,_) -> return (Loc CommandLine name)
 

data Options = Options { includePath :: [FilePath], outputPath :: FilePath }
defaultOptions = Options
  { includePath = defaultIncludePath
  , outputPath = "out"
  }

parseArgs :: [String] -> (Options, [String])
parseArgs args = (opts, mods)
  where
    opts = foldr addOption defaultOptions optargs
    addOption ('-':'I':path) opts =
      opts { includePath = path : includePath opts }
    addOption ('-':'o':path) opts = opts { outputPath = path }
    (optargs,mods) = partition ((== '-').head) args

main = do
  (opts,mods) <- parseArgs <$> getArgs
  mapM_ (doMain opts <=< parseName) mods

doMain opts name = do
  mods <- runMod opts M.empty (processImport name)
  process opts name mods

mapMapM f m = M.fromList <$> mapM (secondM f) (M.toList m)
  where
    secondM f (a,b) = f b >>= \b' -> return (a,b')

process :: Options -> Loc Name -> ModMap -> IO ()
process opts name mods'' = do
  mods' <- mapMapM scopecheck (M.mapMaybe id mods'')
  mods <- runReaderT (typecheck name) mods'
  let outfile = outputPath opts </> encodeName (locData name) ++ ".ll"
  --mapM_ print (M.toList mods)
  runReaderT (printLLVM outfile name) mods
