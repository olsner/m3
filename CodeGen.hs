{-# LANGUAGE NoMonomorphismRestriction,FlexibleContexts,RankNTypes,PatternGuards #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

module CodeGen (printLLVM) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.Char
import Data.List
import Data.Maybe

{-import Data.Set (Set)
import qualified Data.Set as S-}
import Data.Map (Map)
import qualified Data.Map as M

import Data.Generics hiding (Unit)

import Text.Printf

import AST
import Counter
import CppToken
import SetWriter
import TypeCheck (maybeM)

data ValueKind = Variable | ConstExpr | AllocaPtr deriving Show
data Value = Value { valueKind :: ValueKind, valueType :: Type, valueTextNoType :: String } deriving Show
type Locals = Map String Value
type CGMT m = CounterT Int (StateT Locals (WriterT String m))
type CGM a = forall m . (Functor m, MonadIO m) => CGMT m a

cgError :: Location -> String -> m a
cgError loc msg = error ("[CG] "++show loc++": "++msg)

fresh :: CGM String
fresh = printf "%%t%d" <$> getAndInc

freshLabel :: CGM String
freshLabel = printf ".label%d" <$> getAndInc

withLocal :: MonadIO m => Name -> Value -> CGMT m a -> CGMT m a
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

runCGM :: (Functor m, Monad m) => FormalParams -> CGMT m a -> WriterT String m a
runCGM args = fmap fst . flip runStateT (M.fromList $ concatMap f args) . runCounterT 0
  where
    f (FormalParam typ (Just name)) = [(nm, mkValue ConstExpr typ ('%':nm))] where nm = encodeName name
    f _ = []


mapMapM :: Ord k => Monad m => (v -> m v') -> Map k v -> m (Map k v')
mapMapM f = liftM M.fromList . mapM (\(k,v) -> (,) k `liftM` f v) . M.toList

stringfindUnits :: Map Name (Unit TypedE) -> (Map Name (Unit TypedE), Map String Int)
stringfindUnits = runStringFinder . mapMapM getStrings

printLLVM :: (Functor m, MonadIO m, MonadReader (Map Name (Unit TypedE)) m) => Name -> m ()
printLLVM name = do
  output <- execWriterT $ do
    (units, stringMap) <- asks stringfindUnits
    local (const units) $ do
      -- _ <- liftIO (printf "Generating code for %s...\n" (show name))
      writeStrings stringMap
      mapM_ (cgUnit . fromJust . flip M.lookup units) (importedUnits units name)
      cgMain name
  liftIO (writeFile ("out/"++encodeName name ++ ".ll") output)

cgMain mainModule = tell $
    "define external i32 @main(i32, i8**) {\n"++
    printf "\t%%ret = tail call i32(i32,i8**)* @%s (i32 %%0,i8** %%1)\n" (encodeName actualMain)++
    "\tret i32 %ret\n"++
    "}\n"
  where
    mainName = QualifiedName ["main"]
    actualMain = qualifyName mainModule mainName

cgUnit (Unit _ decl@(Loc _ (Decl name _))) = do
  tell ("; Start of unit: "++show name++"\n\n")
  cgDecl (QualifiedName []) decl
  tell ("; End of unit: "++show name++"\n\n")

cgDecl name (Loc loc (Decl local def)) =
  cgDef loc (qualifyName name local) local def

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
encodeType (TStruct fields) = "{"++intercalate "," (map encodeField fields)++"}"
encodeType TNullPtr = encodeType (TPtr TVoid)
encodeType other = error ("encodeType "++show other)

encodeField (Loc _ (_,typ)) = encodeType typ
structFieldOffset _ (TStruct fields) name | Just ix <- findIndex ((==name).fst.locData) fields = return ix
structFieldOffset loc (TPtr t@(TStruct _)) name = structFieldOffset loc t name
structFieldOffset loc typ name = cgError loc ("structFieldOffset: failed looking up "++show name++" in "++show typ)

-- | Encode a formal parameter as a LLVM type (e.g. "i32") or type + name (as "i32 %param")
encodeFormal :: Bool        -- ^ Include variable name for parameter lists?
             -> FormalParam -- ^ Formal parameter
             -> String
encodeFormal True (FormalParam typ (Just name)) = encodeType typ++" %"++encodeName name
encodeFormal _ (FormalParam typ _) = encodeType typ
encodeFormal _ VarargParam = "..."

cgDef loc name local def = case def of
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
    case retT of
      TVoid -> line "ret void"
      _ -> line "unreachable"
    tell "}\n\n"
  (ExternalFunction _linkage ret args) -> do
    tell ("declare "++encodeType ret++" @"++encodeName local++"("++intercalate "," (map (encodeFormal False) args)++")\n")
    tell ("@"++encodeName name++" = alias linker_private "++encodeType (TPtr (TFunction ret args))++" @"++encodeName local++"\n")
  (VarDef (TConst _) e) -> do
    res <- runCGM [] (maybeM cgTypedE e)
    let initVal = case res of
          Just e -> valueText e
          Nothing -> "undef"
    tell ("@"++encodeName name++" = linker_private constant "++initVal++"\n")
  (VarDef t Nothing) -> do
    tell ("@"++encodeName name++" = linker_private global "++encodeType t++" undef\n")
  -- TODO Globals with initializers
  (VarDef _ _) -> cgError loc ("Weird VarDef: "++show def)
  (TypeDef _) -> return ()

cgFunBody :: (Functor m, MonadIO m) => LocStatement TypedE -> CGMT m ()
cgFunBody = cgStmt

infixl 1 =%
locComment msg loc = line ("; "++msg++" @ "++show loc)
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
extractvalue :: Value -> Int -> String
extractvalue base x = "extractvalue "++valueText base++", "++show x
false = intValue 0
zero = intValue 0 TInt
one = intValue 1 TInt
minusOne = intValue (-1) TInt
intValue :: Int -> Type -> Value
intValue i t = mkValue ConstExpr t (show i)
bitcast value to = "bitcast " ++ valueText value ++ " to " ++ encodeType to
trunc value to = "trunc " ++ valueText value ++ " to " ++ encodeType to

withVars :: (Functor m, MonadIO m) => [LocDecl TypedE] -> CGMT m a -> CGMT m a
withVars vars m = foldr withVar m vars
withVar :: (Functor m, MonadIO m) => LocDecl TypedE -> CGMT m a -> CGMT m a
withVar (Loc loc (Decl name def)) m = case def of
    (VarDef (TConst _) init) ->
      cgTypedE (fromJust init) >>= \initVal -> withLocal name initVal m
    (VarDef typ init) ->
      maybeM cgTypedE init >>= \initVal -> do
        let value = mkValue AllocaPtr (TPtr typ) ("%"++encodeName name)
        withLocal name value $ do
          locComment ("Variable "++show name) loc
          value =% alloca typ
          maybeM (\initVal -> store initVal value) initVal
          m
    (TypeDef _) -> m
    _ -> cgError loc ("Unexpected vardef "++show def)

cgStmt :: LocStatement TypedE -> CGM ()
cgStmt (Loc loc stmt) = case stmt of
  EmptyStmt -> return ()
  (ReturnStmt e) -> do
    v <- cgTypedE e
    locComment "ReturnStmt" loc
    ret v
  (ReturnStmtVoid) -> line "ret void"
  (ExprStmt expr) -> cgTypedE expr >> return ()
  CompoundStmt vars xs -> withVars vars (mapM_ cgStmt xs)
  (VarDecl _) -> cgError loc "Internal error: VarDecl in CodeGen!"
  (TypDecl _ _) -> cgError loc "Internal error: TypDecl in CodeGen!"
  (IfStmt cond t f) -> do
    locComment "IfStmt cond" loc
    c <- cgTypedE cond
    tblock <- freshLabel
    fblock <- freshLabel
    end <- freshLabel
    brIf c tblock fblock
    locComment "IfStmt true" loc
    label tblock $ do
      cgStmt t
      br end
    locComment "IfStmt false" loc
    label fblock $ do
      cgStmt f
      br end
    locComment "IfStmt end" loc
    label end (return ())
  (WhileStmt cond body) -> do
    startCond <- freshLabel
    start <- freshLabel
    end <- freshLabel
    br startCond
    locComment "WhileStmt cond" loc
    label startCond $ do
      c <- cgTypedE cond
      brIf c start end
    label start $ do
      locComment "WhileStmt body" loc
      cgStmt body
      locComment "WhileStmt end" loc
      br startCond
    label end (return ())
  --other -> tell ("; UNIMPL!!! "++show other++"\n")

withFresh typ m = fresh >>= \r -> let v = mkValue Variable typ r in v <$ m v

cgTypedE :: TypedE -> CGM Value
cgTypedE (TypedE l t e) = cgExpr l t e
cgExpr :: Location -> Type -> Expr TypedE -> CGM Value
cgExpr loc typ e = case e of
  (EBool b) -> return (mkValue ConstExpr typ (if b then "1" else "0"))
  (EInt i) -> return (mkValue ConstExpr typ (show i))
  (EChar i) -> return (mkValue ConstExpr typ (show (ord i)))
  (EFunCall fun@(TypedE _ funType _) args) -> do
    (fun_:args_) <- mapM cgTypedE (fun:args)
    printloc "EFunCall"
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
  (EArrToPtr (TypedE arrLoc arrT arr)) -> do
    (arrT',arrelem) <- case arrT of
          (TArray _ arrelem) -> return (TPtr arrT, arrelem)
          (TPtr (TArray _ arrelem)) -> return (arrT, arrelem)
          typ -> cgError loc ("EArrToPtr on something not array or ptr-to-array: "++show typ++" in "++show e)
    v <- cgExpr arrLoc arrT' arr
    -- lift (liftIO (printf "EArrToPtr: %s -> %s\n" (show e) (show typ)))
    case valueKind v of
      ConstExpr -> return (mkValue ConstExpr (TPtr arrelem) ("getelementptr ("++valueText v++", i1 0, i1 0)"))
      _ -> printloc "EArrToPtr" >> withFresh (TPtr arrelem) (=% getelementptr v [zero, zero])
  (EDeref loc) -> do
    loc' <- cgTypedE loc
    printloc "EDeref"
    withFresh typ (=% load loc')
  (EAssignment (_,assignOp) lval rval) -> do
    let (TypedE _ typ (EDeref target)) = lval
    lv <- cgTypedE target
    lval <- withFresh typ (=% load lv)
    rv <- cgTypedE rval
    printloc "EAssignment"
    res <- cgAssignOp loc assignOp rv lval
    store res lv
  (EBinary op x y) -> do
    xres <- cgTypedE x
    yres <- cgTypedE y
    printloc ("binary "++show (snd op))
    getBinopCode (snd op) loc typ xres yres
  (EArrayIndex arr@(TypedE _ (TPtr elem) _) ix) -> do
    arr' <- cgTypedE arr
    ix' <- cgTypedE ix
    printloc "EArrayIndex"
    elptr <- withFresh (TPtr elem) (=% getelementptr arr' [ix'])
    withFresh elem (=% load elptr)
  (EFieldAccess name struct@(TypedE _ structT _)) -> do
    v <- cgTypedE struct
    ix <- structFieldOffset loc structT name
    printloc "EFieldAccess"
    case typ of
      TPtr _ -> withFresh typ (=% getelementptr v [zero, intValue ix TInt])
      _ -> withFresh typ (=% extractvalue v ix)
  (EPostfix (_,op) (TypedE _ _ (EDeref lvExpr))) -> do
    printloc ("postfix "++show op)
    lv <- cgTypedE lvExpr
    val <- withFresh typ (=% load lv)
    res <- cgPostfixOp loc op typ val
    store res lv
    return val
  (EUnary op val) -> do
    v <- cgTypedE val
    printloc ("unary "++show (snd op))
    cgUnary typ loc (snd op) v
  (ECast to expr@(TypedE _ from _)) -> do
    lv <- cgTypedE expr
    printloc ("ECast "++show from++" to "++show to)
    cgCast to from lv
  ENullPtr -> return (mkValue ConstExpr TNullPtr "null")
  other -> cgError loc ("Unimplemented expression: "++show other)
  where
    printloc msg = locComment msg loc

icmp op typ x y = withFresh typ (=% unwords ["icmp",op,valueText x++",",valueTextNoType y])
cmpBinop tok op pos typ = case typ of 
  TInt -> icmp op typ
  TBool -> icmp op typ
  TChar -> icmp op typ
  --TFloat -> fcmp op
  _ -> error (show pos++": "++show tok++" operator only supports ints, attempted on "++show typ)
arithBinop tok op loc typ = case typ of
  TInt -> f op
  TChar -> f op
  (TPtr _) -> \x y -> case op of
    "add" -> withFresh typ (=% getelementptr x [y])
    "sub" -> do
      incr <- withFresh TInt (=% "sub "++encodeType (valueType y)++" 0,"++valueTextNoType y)
      withFresh typ (=% getelementptr x [incr])
    _ -> cgError loc ("Unimplemented operator on pointers: "++op)
  _ -> \_ _ -> cgError loc (show tok++" operator only supports ints, attempted on "++show typ)
  where
      f op x y = withFresh typ (=% unwords [op,valueText x,",",valueTextNoType y])
getBinopCode t = case t of
  Equal -> cmpBinop t "eq"
  NotEqual -> cmpBinop t "ne"
  LessThan -> cmpBinop t "slt"
  GreaterOrEqual -> cmpBinop t "sge"
  Plus -> arithBinop t "add"
  Minus -> arithBinop t "sub"
  Asterix -> arithBinop t "mul"
  Division -> arithBinop t "sdiv"
  Modulo -> arithBinop t "srem"
  _ -> error ("getBinopCode: "++show t)

cgAssignOp _ Assignment = \rv _lv -> return rv
cgAssignOp _ PlusAssign = \rv lv -> withFresh (valueType lv) $ (=% "add "++valueText lv++","++valueTextNoType rv)
cgAssignOp loc op = \_ _ -> cgError loc ("Unknown/unimplemented assignment-operator: "++show op)

cgPostfixOp _ Decrement typ@(TPtr _) val = withFresh typ $ (=% getelementptr val [minusOne])
cgPostfixOp _ Increment typ@(TPtr _) val = withFresh typ $ (=% getelementptr val [one])
cgPostfixOp _ Decrement typ val = withFresh typ $ (=% "sub "++valueText val++", 1")
cgPostfixOp _ Increment typ val = withFresh typ $ (=% "add "++valueText val++", 1")
cgPostfixOp loc op _ _ = cgError loc ("Unknown/unimplemented postfix operator: "++show op)

-- The truly no-op case: the source and target (LLVM!) type are the same
cgCast to from lv | encodeType to == encodeType from = return lv
-- Now for the cases with actually different types :)
cgCast to@(TPtr _) (TPtr _) lv = do
  withFresh to (=% bitcast lv to)
cgCast to@(TPtr _) TInt lv = do
  withFresh to (=% "inttoptr "++valueText lv)
cgCast to@TInt (TPtr _) lv = do
  withFresh to (=% "ptrtoint "++valueText lv)
cgCast to@(TPtr _) TNullPtr lv = cgCast to (TPtr TVoid) lv
cgCast to@TInt TChar lv = withFresh to (=% "sext "++valueText lv++" to "++encodeType to)
cgCast to@TInt TBool lv = withFresh to (=% "sext "++valueText lv++" to "++encodeType to)
cgCast to@TChar TInt lv = withFresh to (=% trunc lv to)
cgCast to from _ = error ("cgCast: Unimplemented cast from "++show from++" to "++show to)

cgUnary typ loc LogicalNot = \val -> do
    v <- getBinopCode Equal loc typ (false typ) val
    cgCast typ TBool v
cgUnary typ _ Minus = \val -> withFresh typ (=% "sub "++encodeType typ++" 0, "++valueTextNoType val)
cgUnary typ loc tok = \_ -> cgError loc ("Unhandled unary operator: "++show tok++" for type "++show typ)

{-type VC a = CounterT Int (State (Set Name, Map Name Name)) a
runVC :: VC a -> a
runVC = flip evalState (S.empty,M.empty) . runCounterT 0
renameVariables :: Statement TypedE -> VC (Statement TypedE)
renameVariables = gmapM (mkM f `extM` g)
  where
    f :: Statement TypedE -> VC (Statement TypedE)
    f (CompoundStmt vars stmts) = do
      oldMapping <- gets snd
      mapM_ mapVariable vars
      stmts' <- mapM renameVariables stmts
      modify (second (const oldMapping))
      return (CompoundStmt vars stmts')
    f x = renameVariables x
    mapVariable (_,name,_) = do
      existingMapping <- gets (S.member name . fst)
      newName <- case existingMapping of
        True -> qualifyName1 name . show <$> getAndInc
        False -> return name
      modify (first (S.insert newName))
      modify (second (M.insert name newName))
    g :: TypedE -> VC TypedE
    g (TypedE t (EVarRef name)) = TypedE t . EVarRef <$> gets (fromMaybe name . M.lookup name . snd)
    g x = return x
-}

type SF a = CounterT Int (SetWriter (Map String Int)) a
runStringFinder :: SF a -> (a,Map String Int)
runStringFinder = runSetWriter M.empty . runCounterT 0
getStringMap :: SF (Map String Int)
getStringMap = lift (listen (return ()) >>= \((),w) -> return w) 
hasString s = M.member s <$> getStringMap
stringReplacement loc str = do
    b <- hasString str
    i <- if b then getString else newString
    let arrType = TArray (length str+1) TChar
    return $
      TypedE loc (TPtr TChar) $
      EArrToPtr $ TypedE loc arrType $
        EVarRef (QualifiedName [printf ".STR%d" i])
  where
    getString :: SF Int
    getString = fromJust . M.lookup str <$> getStringMap
    newString :: SF Int
    newString = getAndInc >>= \i ->
      lift (tell (M.fromList [(str,i)])) >> return i

getStrings = everywhereM (mkM f)
  where
    f (TypedE loc _ (EString str)) = stringReplacement loc str
    f x = return x

showStringLLVM xs = "\""++(f =<< xs)++"\""
  where
    f x
      | elem x "\"" || not (isPrint x) = "\\" ++ printf "%02x" (ord x)
      | otherwise = [x]
writeStrings = mapM_ (tell . makeString) . M.toList
  where
    makeString (str,i) = printf "@.STR%d = internal constant [%d x i8] c%s\n" i (length str+1) (showStringLLVM (str++"\0"))
