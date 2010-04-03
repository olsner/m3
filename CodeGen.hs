{-# LANGUAGE NoMonomorphismRestriction,ScopedTypeVariables,ExistentialQuantification,FlexibleContexts #-}

module CodeGen (printLLVM) where

import Control.Applicative
import Control.Monad(liftM, when)
import Control.Monad.Writer

import Data.Int
import Data.List

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

type CGM a = CounterT Int (Writer String) a

fresh :: CGM String
fresh = printf "%%%d" <$> getAndInc

printLLVM name unit = do
  writeFile (encodeName name ++ ".ll") (execWriter $ cgDecl name (unitDecl unit))

cgDecl name (Decl local def) =
  cgDef (qualifyName name local) def

encodeType :: Type -> String
encodeType (TPtr TVoid) = "i8*"
encodeType (TPtr t) = encodeType t++"*"
encodeType TInt = "i32"
encodeType TChar = "i8"
encodeType TVoid = "void"
encodeType (TConst t) = encodeType t
encodeType t = "i32 ; "++show t++"\n"

encodeFormal VarargParam = "..."
encodeFormal (FormalParam typ name) = encodeType typ

cgDef name def = case def of
  (ModuleDef decls) -> mapM_ (cgDecl name) decls
  (FunctionDef retT args code) -> runCounterT 0 $ do
  	tell "define linker_private_linkage "
	tell (encodeType retT ++ " ")
  	tell (encodeName name ++ " ")
	tell "("
	tell (intercalate "," (map encodeFormal args))
	tell ")"
	tell "{\n"
	cgFunBody code -- TODO With environment! Must map M3 names to LLVM registers...
	tell "}\n\n"
  (ExternalFunction linkage ret args) -> tell ("declare @"++encodeName name++"("++intercalate "," (map encodeFormal args)++")\n")

cgFunBody = cgStmts

cgStmts :: [Statement] -> CGM ()
cgStmts code = mapM_ cgStmt code

cgStmt :: Statement -> CGM ()
cgStmt stmt = case stmt of
  EmptyStmt -> return ()
  -- TODO Need to know the type of 'e'
  (ReturnStmt e) -> cgExpr e >>= \e -> tell ("ret "++(encodeType TInt)++" "++e++"\n")
  (ReturnStmtVoid) -> tell "ret void\n"
  (ExprStmt expr) -> cgExpr expr >> return ()
  other -> tell ("; UNIMPL!!! "++show other++"\n")

withFresh m = fresh >>= \r -> m r >> return r

-- TODO cgExpr :: Expr -> CGM RegName -- make it return the name of where the result was stored. Also somehow memoize the result so that Expr's with sharing have the same sharing - may require clever changes to Expr.
cgExpr (EInt i) = withFresh $ \r -> tell (r++" = i32 "++show i++"\n")
--cgExpr (EString s) = createStringNul s
cgExpr (EFunCall fun args) = do
  (fun_:args_) <- mapM cgExpr (fun:args)
  r <- fresh
  tell (r ++ " = call "++encodeType TInt++" "++fun_++"("++intercalate "," args_++")"++"\n")
  return r
cgExpr (EVarRef name) = fresh >>= \r -> tell (r ++" = load @"++encodeName name++"\n") >> return r
cgExpr (EAssignment assignType lval rval) = do
  rv <- cgExpr rval
  lv <- cgExpr lval
  tell ("store "++lv++", "++rv++"\n")
  return rv
cgExpr other = tell ("; UNIMPL!!! "++show other++"\n") >> fresh

{-    EVarRef Name
  | EFunCall Expr [Expr]
  | EBinary Tok Expr Expr
  | EUnary Tok Expr
  | EConditional Expr Expr Expr -- ^ ?: expressions: condition, true-value, false-value
  | EAssignment Tok Expr Expr
  | EString String
  | EInt Integer -}
