{-# LANGUAGE NoMonomorphismRestriction,ScopedTypeVariables,ExistentialQuantification,FlexibleContexts,RankNTypes #-}

module CodeGen (printLLVM) where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.Char
import Data.List
import Data.Maybe

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

import Text.Printf

{-import LLVM.Core hiding (createNamedFunction, newNamedFunction, defineFunction)
import qualified LLVM.Core.Util as U

import qualified LLVM.FFI.Core as FFI
import qualified LLVM.FFI.Target as FFI
import qualified LLVM.FFI.BitWriter as FFI
import qualified LLVM.FFI.BitReader as FFI
import qualified LLVM.FFI.Transforms.Scalar as FFI-}

import AST
import Counter
import CppToken
import SetWriter

type Locals = Set String
type CGMT m = CounterT Int (StateT Locals (WriterT String m))
type CGM a = forall m . (MonadIO m) => CGMT m a

fresh :: CGM String
fresh = printf "%%%d" <$> getAndInc

withLocal :: String -> CGM a -> CGM a
withLocal str m = do
  s <- get
  modify (S.insert str)
  r <- m
  put s
  return r
isLocal :: String -> CGM Bool
isLocal str = gets (S.member str)

runCGM :: Monad m => FormalParams -> CGMT m a -> WriterT String m a
runCGM args = fmap fst . flip runStateT S.empty {- TODO Names from args... -} . runCounterT (length args+1)

mapMapM :: Ord k => Monad m => (v -> m v') -> Map k v -> m (Map k v')
mapMapM f = liftM M.fromList . mapM (\(k,v) -> (,) k `liftM` f v) . M.toList

stringfindUnits :: Map Name (Unit TypedE) -> (Map Name (Unit TypedE), Map String Int)
stringfindUnits = runStringFinder . mapMapM getStrings

printLLVM :: (MonadIO m, MonadReader (Map Name (Unit TypedE)) m) => Name -> m ()
printLLVM name = do
  output <- execWriterT $ do
    (units, stringMap) <- asks stringfindUnits
    local (const units) $ do
      liftIO (printf "Generating code for %s...\n" (show name))
      writeStrings stringMap
      mapM_ (cgUnit . fromJust . flip M.lookup units) (importedUnits units name)
      cgMain name
  liftIO (writeFile (encodeName name ++ ".ll") output)

cgMain mainModule = tell $
    "define external i32 @main(i32, i8**) {\n"++
    printf "\t%%ret = tail call i32(i32,i8**)* @%s (i32 %%0,i8** %%1)\n" (encodeName actualMain)++
    "\tret i32 %ret\n"++
    "}\n"
  where
    mainName = QualifiedName ["main"]
    actualMain = qualifyName mainModule mainName

cgUnit (Unit _ decl@(Decl name _)) = do
  tell ("; Start of unit: "++show name++"\n\n")
  cgDecl (QualifiedName []) decl
  tell ("; End of unit: "++show name++"\n\n")

cgDecl name (Decl local def) =
  cgDef (qualifyName name local) local def

encodeType :: Type -> String
encodeType (TPtr TVoid) = "i8*"
encodeType (TPtr t) = encodeType t++"*"
encodeType TInt = "i32"
encodeType TChar = "i8"
encodeType TBool = "i1"
encodeType TVoid = "void"
encodeType (TConst t) = encodeType t
encodeType (TFunction t params) = encodeType t++" ("++intercalate "," (map encodeFormal params)++")"
encodeType (TArray len typ) = "["++show len++" x "++encodeType typ++"]"

encodeFormal VarargParam = "..."
encodeFormal (FormalParam typ _) = encodeType typ

cgDef name local def = case def of
  (ModuleDef decls) -> mapM_ (cgDecl name) decls
  (FunctionDef retT args code) -> runCGM args $ do
    tell "define linker_private "
    tell (encodeType retT ++ " ")
    tell ("@"++encodeName name ++ " ")
    tell "("
    tell (intercalate "," (map encodeFormal args))
    tell ")"
    tell "{\n"
    cgFunBody code -- TODO With environment! Must map M3 names to LLVM registers...
    tell "}\n\n"
  (ExternalFunction _linkage ret args) -> do
    tell ("declare "++encodeType ret++" @"++encodeName local++"("++intercalate "," (map encodeFormal args)++")\n")
    tell ("@"++encodeName name++" = alias "++encodeType (TPtr (TFunction ret args))++" @"++encodeName local++"\n")

cgFunBody = cgStmts

cgStmts :: [Statement TypedE] -> CGM ()
cgStmts code = mapM_ cgStmt code

cgStmt :: Statement TypedE -> CGM ()
cgStmt stmt = case stmt of
  EmptyStmt -> return ()
  (ReturnStmt e) -> tell . printf "ret %s\n" =<< cgTypedE e
  (ReturnStmtVoid) -> tell "ret void\n"
  (ExprStmt expr) -> cgTypedE expr >> return ()
  CompoundStmt xs -> mapM_ cgStmt xs
  (VarDecl name typ x) -> withLocal (encodeName name) $ do
    tell ("%"++encodeName name++" = alloca "++encodeType typ++"\n")
    cgStmt x
  --other -> tell ("; UNIMPL!!! "++show other++"\n")

withFresh typ m = fresh >>= \r -> m r >> return (encodeType typ ++ " " ++ r)

cgTypedE (TypedE t e) = cgExpr t e
-- TODO Somehow memoize the result so that Expr's with sharing have the same sharing - may require clever changes to Expr.
cgExpr typ e = case e of
  (EBool b) -> return ("i1 "++if b then "1" else "0")
  (EInt i) -> return ("i32 "++show i)
  (EFunCall fun@(TypedE funType _) args) -> do
    (fun_:args_) <- mapM cgTypedE (fun:args)
    let funcall = "call "++fun_++"("++intercalate "," args_++")"++"\n"
    let TPtr (TFunction retT _) = funType
    if retT == TVoid
      then tell funcall >> return (error "void function result used by something! In CodeGen!") -- The return value should be guaranteed unused!
      else withFresh retT $ \r -> tell (r ++ " = "++funcall)
  (EVarRef name) -> do
    let nm = encodeName name
    local <- isLocal nm
    return (encodeType typ++(if local then " %" else " @")++nm)
  (EArrToPtr (TypedE arrT@(TArray _ arrelem) arr)) -> do
    v <- cgExpr (TPtr arrT) arr
    return (encodeType (TPtr arrelem)++" getelementptr ("++v++", i1 0, i1 0)")
  (EDeref loc) -> withFresh typ $ \r -> do
    loc' <- cgTypedE loc
    tell (r++" = load "++loc'++"\n")
  (EAssignment (_,Assignment) lval rval) -> do
    let (TypedE _ (EDeref loc)) = lval
    lv <- cgTypedE loc
    rv <- cgTypedE rval
    tell ("store "++rv++", "++lv++"\n")
    return rv
  other -> error ("Unimplemented expression: "++show other)

{-    EVarRef Name
  | EFunCall Expr [Expr]
  | EBinary Tok Expr Expr
  | EUnary Tok Expr
  | EConditional Expr Expr Expr -- ^ ?: expressions: condition, true-value, false-value
  | EAssignment Tok Expr Expr
  | EString String
  | EInt Integer -}

type SF a = CounterT Int (SetWriter (Map String Int)) a
runStringFinder :: SF a -> (a,Map String Int)
runStringFinder = runSetWriter M.empty . runCounterT 0
getStringMap :: SF (Map String Int)
getStringMap = lift (listen (return ()) >>= \((),w) -> return w) 
hasString s = M.member s <$> getStringMap
stringReplacement str = do
    b <- hasString str
    i <- if b then getString else newString
    let arrType = TArray (length str+1) TChar
    return $
      TypedE (TPtr TChar) $
      EArrToPtr $ TypedE arrType $
        EVarRef (QualifiedName [printf ".STR%d" i])
  where
    getString :: SF Int
    getString = fromJust . M.lookup str <$> getStringMap
    newString :: SF Int
    newString = getAndInc >>= \i ->
      lift (tell (M.fromList [(str,i)])) >> return i

getStrings :: Unit TypedE -> SF (Unit TypedE)
getStrings unit = do
    decl <- getStringsDecl (unitDecl unit)
    return (unit { unitDecl = decl })
getStringsDecl (Decl local def) = Decl local <$> getStringsDef def
getStringsDef (ModuleDef decls) = ModuleDef <$> mapM getStringsDecl decls
getStringsDef (FunctionDef retT args code) = FunctionDef retT args <$> mapM getStringsStmt code
getStringsDef def = return def
getStringsStmt (ReturnStmt e) = ReturnStmt <$> getStringsTypedE e
getStringsStmt (ExprStmt e) = ExprStmt <$> getStringsTypedE e
getStringsStmt (VarDecl n t s) = VarDecl n t <$> getStringsStmt s
getStringsStmt (CompoundStmt ss) = CompoundStmt <$> mapM getStringsStmt ss
getStringsStmt s = error (show s)

getStringsTypedE (TypedE _ (EString str)) = stringReplacement str
getStringsTypedE (TypedE t e) = TypedE t <$> getStringsExpr e
-- This is the one with the action
-- These patterns are just about finding the EString expressions
getStringsExpr (EFunCall fun args) = EFunCall <$> getStringsTypedE fun <*> mapM getStringsTypedE args
getStringsExpr (EAssignment op lval rval) = EAssignment op <$> getStringsTypedE lval <*> getStringsTypedE rval
getStringsExpr (EDeref e) = EDeref <$> getStringsTypedE e
getStringsExpr e@(EVarRef _) = return e
getStringsExpr e@(EInt _) = return e
getStringsExpr e@(EBool _) = return e
getStringsExpr e = error ("Unhandled expression in string finder: "++show e)

showStringLLVM xs = "\""++(f =<< xs)++"\""
  where
    f x
      | isPrint x = [x]
      | otherwise = "\\" ++ printf "%02x" (ord x)
writeStrings = mapM (tell . makeString) . M.toList
  where
    makeString (str,i) = printf "@.STR%d = internal constant [%d x i8] c%s\n" i (length str+1) (showStringLLVM (str++"\0"))
