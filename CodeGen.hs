{-# LANGUAGE NoMonomorphismRestriction,ScopedTypeVariables,ExistentialQuantification,FlexibleContexts,RankNTypes #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

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
import TypeCheck (maybeM)

data ValueKind = Variable | ConstExpr | AllocaPtr
data Value = Value { valueKind :: ValueKind, valueType :: Type, valueTextNoType :: String }
type Locals = Map String Value
type CGMT m = CounterT Int (StateT Locals (WriterT String m))
type CGM a = forall m . (MonadIO m) => CGMT m a

fresh :: CGM String
fresh = printf "%%t%d" <$> getAndInc

freshLabel :: CGM String
freshLabel = printf ".label%d" <$> getAndInc

withLocal :: Name -> Value -> CGM a -> CGM a
withLocal name val m = do
  s <- get
  modify (M.insert (encodeName name) val)
  r <- m
  put s
  return r
getLocal :: Name -> CGM (Maybe Value)
getLocal name = gets (M.lookup (encodeName name))

mkValue :: ValueKind -> Type -> String -> Value
mkValue k typ s = Value k typ s
valueText v = encodeType (valueType v)++' ':valueTextNoType v

runCGM :: Monad m => FormalParams -> CGMT m a -> WriterT String m a
runCGM args = fmap fst . flip runStateT (M.fromList $ concatMap f args) . runCounterT 0
  where
    f (FormalParam typ (Just name)) = [(nm, mkValue ConstExpr typ ('%':nm))] where nm = encodeName name
    f _ = []


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
encodeType (TPtr (TConst t)) = encodeType (TPtr t)
encodeType (TPtr TVoid) = "i8*"
encodeType (TPtr t) = encodeType t++"*"
encodeType TInt = "i32"
encodeType TChar = "i8"
encodeType TBool = "i1"
encodeType TVoid = "void"
encodeType (TConst t) = encodeType t
encodeType (TFunction t params) = encodeType t++"("++intercalate "," (map (encodeFormal False) params)++")"
encodeType (TArray len typ) = "["++show len++" x "++encodeType typ++"]"
encodeType TNullPtr = encodeType (TPtr TVoid)
encodeType other = error ("encodeType "++show other)

-- | Encode a formal parameter as a LLVM type (e.g. "i32") or type + name (as "i32 %param")
encodeFormal :: Bool        -- ^ Include variable name for parameter lists?
             -> FormalParam -- ^ Formal parameter
             -> String
encodeFormal True (FormalParam typ (Just name)) = encodeType typ++" %"++encodeName name
encodeFormal _ (FormalParam typ _) = encodeType typ
encodeFormal _ VarargParam = "..."

cgDef name local def = case def of
  (ModuleDef decls) -> mapM_ (cgDecl name) decls
  (FunctionDef retT args code) -> runCGM args $ do
    tell "define linker_private "
    tell (encodeType retT ++ " ")
    tell ("@"++encodeName name ++ " ")
    tell "("
    tell (intercalate "," (map (encodeFormal True) args))
    tell ")"
    tell "{\n"
    cgFunBody code
    line "unreachable"
    tell "}\n\n"
  (ExternalFunction _linkage ret args) -> do
    tell ("declare "++encodeType ret++" @"++encodeName local++"("++intercalate "," (map (encodeFormal False) args)++")\n")
    tell ("@"++encodeName name++" = alias linker_private "++encodeType (TPtr (TFunction ret args))++" @"++encodeName local++"\n")
  (VarDef (TConst t) e) -> do
    res <- runCGM [] (maybeM cgTypedE e)
    let initVal = case res of
          Just e -> valueText e
          Nothing -> "undef"
    tell ("@"++encodeName name++" = linker_private constant "++initVal++"\n")
  (VarDef t Nothing) -> do
    tell ("@"++encodeName name++" = global linker_private "++encodeType t++" undef\n")

cgFunBody = cgStmts

cgStmts :: [Statement TypedE] -> CGM ()
cgStmts code = mapM_ cgStmt code

infixl 1 =%
line str = tell ('\t':str++"\n")
value =% expr = line (valueTextNoType value++" = "++expr)
alloca typ = "alloca "++encodeType typ
load src = "load "++valueText src
store src dst = line ("store "++valueText src++", "++valueText dst) >> return src
ret val = line ("ret "++valueText val)
valueTextList = intercalate ", " . map valueText
call fun args = "call "++valueText fun++"("++valueTextList args++")"
br target = line ("br label %"++target)
brIf cond true false = line ("br "++valueText cond++", label %"++true++", label %"++false)
label lbl m = tell (lbl++":\n") >> m
getelementptr base xs = "getelementptr "++valueText base++","++valueTextList xs
zero = mkValue ConstExpr TBool "0"

cgStmt :: Statement TypedE -> CGM ()
cgStmt stmt = case stmt of
  EmptyStmt -> return ()
  (ReturnStmt e) -> ret =<< cgTypedE e
  (ReturnStmtVoid) -> line "ret void"
  (ExprStmt expr) -> cgTypedE expr >> return ()
  CompoundStmt xs -> mapM_ cgStmt xs
  (VarDecl name typ init x) -> maybeM cgTypedE init >>= \initVal -> do
      case typ of
        (TConst _) -> withLocal name (fromJust initVal) $ cgStmt x
        _          ->
          let value = mkValue AllocaPtr (TPtr typ) ("%"++encodeName name) in
          withLocal name value $ do
            value =% alloca typ
            maybeM (\initVal -> store initVal value) initVal
            cgStmt x
  (IfStmt cond t f) -> do
    c <- cgTypedE cond
    tblock <- freshLabel
    fblock <- freshLabel
    end <- freshLabel
    brIf c tblock fblock
    label tblock $ do
      cgStmt t
      br end
    label fblock $ do
      cgStmt f
      br end
    label end (return ())
  (WhileStmt cond body) -> do
    startCond <- freshLabel
    start <- freshLabel
    end <- freshLabel
    br startCond
    label startCond $ do
      c <- cgTypedE cond
      brIf c start end
    label start $ do
      cgStmt body
      br startCond
    label end (return ())
  --other -> tell ("; UNIMPL!!! "++show other++"\n")

withFresh typ m = fresh >>= \r -> let v = mkValue Variable typ r in v <$ m v

cgTypedE (TypedE t e) = cgExpr t e
cgExpr :: Type -> Expr TypedE -> CGM Value
cgExpr typ e = case e of
  (EBool b) -> return (mkValue ConstExpr typ (if b then "1" else "0"))
  (EInt i) -> return (mkValue ConstExpr typ (show i))
  (EChar i) -> return (mkValue ConstExpr typ (show (ord i)))
  (EFunCall fun@(TypedE funType _) args) -> do
    (fun_:args_) <- mapM cgTypedE (fun:args)
    let funcall = call fun_ args_
    let TPtr (TFunction retT _) = funType
    if retT == TVoid
      then line funcall >> return (error "void function result used by something! In CodeGen!") -- The return value should be guaranteed unused!
      else withFresh retT (=% funcall)
  (EVarRef name) -> do
    local <- getLocal name
    case local of
      (Just val) -> return val
      _ -> return (mkValue ConstExpr typ ('@':encodeName name))
  (EArrToPtr (TypedE arrT arr)) -> do
    let (arrT',arrelem) = case arrT of
          (TArray _ arrelem) -> (arrT, arrelem)
          (TPtr arrT@(TArray _ arrelem)) -> (arrT, arrelem)
    v <- cgExpr (TPtr arrT) arr
    lift (liftIO (printf "EArrToPtr: %s -> %s\n" (show e) (show typ)))
    case valueKind v of
      ConstExpr -> return (mkValue ConstExpr (TPtr arrelem) ("getelementptr ("++valueText v++", i1 0, i1 0)"))
      k -> withFresh (TPtr arrelem) (=% getelementptr v [zero, zero])
  (EDeref loc) -> do
    loc' <- cgTypedE loc
    withFresh typ (=% load loc')
  (EAssignment (_,assignOp) lval rval) -> do
    let (TypedE _ (EDeref loc)) = lval
    lv <- cgTypedE loc
    rv <- cgTypedE rval
    cgAssignOp assignOp rv lv
  (EBinary op x y) -> do
    xres <- cgTypedE x
    yres <- cgTypedE y
    withFresh typ (=% getBinopCode (snd op) (fst op) typ xres yres)
  (EArrayIndex arr@(TypedE (TPtr elem) _) ix) -> do
    arr' <- cgTypedE arr
    ix' <- cgTypedE ix
    elptr <- withFresh (TPtr elem) (=% getelementptr arr' [ix'])
    withFresh elem (=% load elptr)
  (EPostfix op lval) -> do
    let (TypedE _ (EDeref loc)) = lval
    lv <- cgTypedE loc
    val <- withFresh typ (=% load lv)
    res <- cgPostfixOp op typ val
    store res lv
    return val
  (ECast to expr@(TypedE from _)) -> do
    lv <- cgTypedE expr
    cgCast to from lv
  ENullPtr -> return (mkValue ConstExpr TNullPtr "null")
  other -> error ("Unimplemented expression: "++show other)

icmp op x y = unwords ["icmp",op,valueText x++",",valueTextNoType y]
cmpBinop tok op pos typ = case typ of 
  TInt -> icmp op
  TBool -> icmp op
  TChar -> icmp op
  --TFloat -> fcmp op
  _ -> error (show pos++": "++show tok++" operator only supports ints, attempted on "++show typ)
arithBinop tok op pos typ = case typ of
  TInt -> f op
  TChar -> f op
  (TPtr t) -> \x y -> getelementptr x [y]
  _ -> error (show pos++": "++show tok++" operator only supports ints, attempted on "++show typ)
  where
    f op x y = unwords [op,valueText x,valueTextNoType y]
getBinopCode t = case t of
  Equal -> cmpBinop t "eq"
  NotEqual -> cmpBinop t "ne"
  LessThan -> cmpBinop t "slt"
  GreaterOrEqual -> cmpBinop t "sge"
  Plus -> arithBinop t "add"
  Minus -> arithBinop t "sub"
  _ -> error ("getBinopCode: "++show t)

cgAssignOp Assignment = store
cgAssignOp PlusAssign = \rv lv -> do
  lv' <- withFresh (valueType lv) $ (=% "add "++valueText lv++","++valueTextNoType rv)
  store lv' lv

cgPostfixOp (_,Decrement) typ val = withFresh typ $ (=% "sub "++valueText val++", 1")
cgPostfixOp (_,Increment) typ val = withFresh typ $ (=% "add "++valueText val++", 1")

cgCast to@(TPtr _) (TPtr _) lv = do
  withFresh to (=% "bitcast "++valueText lv)
cgCast to@(TPtr _) TInt lv = do
  withFresh to (=% "inttoptr "++valueText lv)
cgCast to@TInt (TPtr _) lv = do
  withFresh to (=% "ptrtoint "++valueText lv)
cgCast to@(TPtr _) TNullPtr lv = cgCast to (TPtr TVoid) lv
cgCast to@TInt TChar lv = withFresh to (=% "signext "++valueText lv)
cgCast to from _ = error ("Unimplemented cast from "++show from++" to "++show to)

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
getStringsStmt (VarDecl n t init s) = VarDecl n t <$> maybeM getStringsTypedE init <*> getStringsStmt s
getStringsStmt (CompoundStmt ss) = CompoundStmt <$> mapM getStringsStmt ss
getStringsStmt (IfStmt c t f) = IfStmt <$> getStringsTypedE c <*> getStringsStmt t <*> getStringsStmt f
getStringsStmt (WhileStmt c s) = WhileStmt <$> getStringsTypedE c <*> getStringsStmt s
getStringsStmt s@(EmptyStmt) = return s
getStringsStmt s@(ReturnStmtVoid) = return s

getStringsTypedE (TypedE _ (EString str)) = stringReplacement str
getStringsTypedE (TypedE t e) = TypedE t <$> getStringsExpr e
-- This is the one with the action
-- These patterns are just about finding the EString expressions
getStringsExpr (EFunCall fun args) = EFunCall <$> getStringsTypedE fun <*> mapM getStringsTypedE args
getStringsExpr (EAssignment op lval rval) = EAssignment op <$> getStringsTypedE lval <*> getStringsTypedE rval
getStringsExpr (EBinary op x y) = EBinary op <$> getStringsTypedE x <*> getStringsTypedE y
getStringsExpr (EPostfix op lval) = EPostfix op <$> getStringsTypedE lval
getStringsExpr (EArrToPtr e) = EArrToPtr <$> getStringsTypedE e
getStringsExpr (EArrayIndex e ix) = EArrayIndex <$> getStringsTypedE e <*> getStringsTypedE ix
getStringsExpr (EDeref e) = EDeref <$> getStringsTypedE e
getStringsExpr (ECast t e) = ECast t <$> getStringsTypedE e
getStringsExpr e@(EVarRef _) = return e
getStringsExpr e@(EInt _) = return e
getStringsExpr e@(EBool _) = return e
getStringsExpr e@(EChar _) = return e
getStringsExpr e@(ENullPtr) = return e
getStringsExpr e = error ("Unhandled expression in string finder: "++show e)

showStringLLVM xs = "\""++(f =<< xs)++"\""
  where
    f x
      | elem x "\"" || not (isPrint x) = "\\" ++ printf "%02x" (ord x)
      | otherwise = [x]
writeStrings = mapM_ (tell . makeString) . M.toList
  where
    makeString (str,i) = printf "@.STR%d = internal constant [%d x i8] c%s\n" i (length str+1) (showStringLLVM (str++"\0"))
