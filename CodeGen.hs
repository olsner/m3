{-# LANGUAGE NoMonomorphismRestriction,ScopedTypeVariables,ExistentialQuantification,FlexibleContexts #-}

module CodeGen (printLLVM) where

import Control.Applicative
import Control.Monad(liftM, when)
import Control.Monad.Writer

import Data.Char
import Data.Int
import Data.List
import Data.Maybe

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

import Numeric

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
import SetWriter

type CGM a = CounterT Int (Writer String) a

fresh :: CGM String
fresh = printf "%%%d" <$> getAndInc

printLLVM :: Name -> Unit TypedE -> IO ()
printLLVM name unit = writeFile (encodeName name ++ ".ll") . execWriter $ do
  let (unit', stringMap) = runStringFinder $ getStrings unit
  writeStrings stringMap
  cgDecl name (unitDecl unit')

cgDecl name (Decl local def) =
  cgDef (qualifyName name local) def

encodeType :: Type -> String
encodeType (TPtr TVoid) = "i8*"
encodeType (TPtr t) = encodeType t++"*"
encodeType TInt = "i32"
encodeType TChar = "i8"
encodeType TVoid = "void"
encodeType (TConst t) = encodeType t
encodeType (TFunction t params) = encodeType t++" ("++intercalate "," (map encodeFormal params)++")"
encodeType (TArray len typ) = "["++show len++" x "++encodeType typ++"]"
encodeType t = "i32 ; "++show t++"\n"

encodeFormal VarargParam = "..."
encodeFormal (FormalParam typ name) = encodeType typ

cgDef name def = case def of
  (ModuleDef decls) -> mapM_ (cgDecl name) decls
  (FunctionDef retT args code) -> runCounterT (length args+1) $ do
    tell "define external "
    tell (encodeType retT ++ " ")
    tell ("@"++encodeName name ++ " ")
    tell "("
    tell (intercalate "," (map encodeFormal args))
    tell ")"
    tell "{\n"
    cgFunBody code -- TODO With environment! Must map M3 names to LLVM registers...
    tell "}\n\n"
  (ExternalFunction linkage ret args) -> tell ("declare "++encodeType ret++" @"++encodeName name++"("++intercalate "," (map encodeFormal args)++")\n")

cgFunBody = cgStmts

cgStmts :: [Statement TypedE] -> CGM ()
cgStmts code = mapM_ cgStmt code

cgStmt :: Statement TypedE -> CGM ()
cgStmt stmt = case stmt of
  EmptyStmt -> return ()
  -- TODO Need to know the type of 'e'
  (ReturnStmt e) -> tell . printf "ret %s\n" =<< cgTypedE e
  (ReturnStmtVoid) -> tell "ret void\n"
  (ExprStmt expr) -> cgTypedE expr >> return ()
  other -> tell ("; UNIMPL!!! "++show other++"\n")

withFresh typ m = fresh >>= \r -> m r >> return (encodeType typ ++ " " ++ r)

cgArgs = zipWith (\(TypedE typ _) v -> encodeType typ++" "++v)

cgTypedE (TypedE t e) = cgExpr t e
-- TODO cgExpr :: Expr -> CGM RegName -- make it return the name of where the result was stored. Also somehow memoize the result so that Expr's with sharing have the same sharing - may require clever changes to Expr.
cgExpr typ e = case e of
  (EInt i) -> return ("i32 "++show i)
  (EFunCall fun@(TypedE funType _) args) -> do
    (fun_:args_) <- mapM cgTypedE (fun:args)
    let funcall = "call "++fun_++"("++intercalate "," args_++")"++"\n"
    let TFunction retT _ = funType
    if retT == TVoid
      then tell funcall >> return undefined -- The return value should be guaranteed unused!
      else withFresh retT $ \r -> tell (r ++ " = "++funcall)
  (EVarRef name) -> return (encodeType (TPtr typ)++" @"++encodeName name)
  (EArrToPtr (TypedE arrT@(TArray _ elem) arr)) -> do
    v <- cgExpr arrT arr
    return (encodeType (TPtr elem)++" getelementptr ("++v++", i1 0, i1 0)")
  (EAssignment assignType lval rval) -> do
    rv <- cgTypedE rval
    lv <- cgTypedE lval
    tell ("store "++lv++", "++rv++"\n")
    return rv
  other -> tell ("; UNIMPL!!! "++show other++"\n") >> fresh

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
    i <- if b then get else new
    return $
      TypedE (TPtr TChar) $
      EArrToPtr $ TypedE (TArray (length str+1) TChar) $
        EVarRef (QualifiedName [printf ".STR%d" i])
  where
    get :: SF Int
    get = fromJust . M.lookup str <$> getStringMap
    new :: SF Int
    new = getAndInc >>= \i ->
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
getStringsStmt s = return s

getStringsTypedE (TypedE _ (EString str)) = stringReplacement str
getStringsTypedE (TypedE t e) = TypedE t <$> getStringsExpr e
-- This is the one with the action
-- These patterns are just about finding the EString expressions
getStringsExpr (EFunCall fun args) = EFunCall <$> getStringsTypedE fun <*> mapM getStringsTypedE args
getStringsExpr e = return e

showStringLLVM xs = "\""++go xs++"\""
  where
    go [] = []
    go (x:xs)
      | isPrint x = x:go xs
      | otherwise = "\\" ++ printf "%02x" (ord x) ++ go xs
writeStrings = mapM (tell . makeString) . M.toList
  where
    makeString (str,i) = printf "@.STR%d = internal constant [%d x i8] c%s\n" i (length str+1) (showStringLLVM (str++"\0"))
