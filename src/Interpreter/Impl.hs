module Interpreter.Impl
       (
         Value(..)
       , runExpr
       -- You may include additional exports here, if you want to
       -- write unit tests for them.
       )
       where

import SubsAst

-- You might need the following imports
import Control.Monad
import qualified Control.Monad.Fail as Fail
import qualified Data.Map as Map
import Data.Map(Map)


-- | A value is either an integer, the special constant undefined,
--   true, false, a string, or an array of values.
-- Expressions are evaluated to values.
data Value = IntVal Int
           | UndefinedVal
           | TrueVal | FalseVal
           | StringVal String
           | ArrayVal [Value]
           deriving (Eq, Show)


type Error = String
type Env = Map Ident Value
type Primitive = [Value] -> Either Error Value
type PEnv = Map FunName Primitive
type Context = (Env, PEnv)



initialContext :: Context
initialContext = (Map.empty, initialPEnv)
  where initialPEnv =
          Map.fromList [ ("===", pEq)
                       , ("<", pLt)
                       , ("+", pAdd)
                       , ("*", pMul)
                       , ("-", pSub)
                       , ("%", pMod)
                       , ("Array", mkArray)
                       ]

newtype SubsM a = SubsM {runSubsM :: Context -> Either Error (a, Env)}

instance Monad SubsM where
  return x = SubsM $ \(env, _) -> Right (x, env)
  m >>= f = SubsM $ \(env, pEnv) -> case runSubsM m (env, pEnv) of
                                      Left e -> Left e
                                      Right (a, env') -> case runSubsM (f a)
                                                              (env', pEnv) of
                                                          Left e -> Left e
                                                          Right v -> Right v
  fail = Fail.fail -- Added as fix for GHC 8.6+

-- Added as fix for GHC 8.6+
instance Fail.MonadFail SubsM where
  fail s = SubsM $ \_ -> Left s


-- You may modify these if you want, but it shouldn't be necessary
instance Functor SubsM where
  fmap = liftM
instance Applicative SubsM where
  pure = return
  (<*>) = ap

pEq :: Primitive
pEq [UndefinedVal, UndefinedVal] = return TrueVal
pEq [TrueVal, TrueVal] = return TrueVal
pEq [FalseVal, FalseVal] = return TrueVal
pEq [IntVal int1, IntVal int2] = if int1 == int2 then return TrueVal
                                 else return FalseVal
pEq [StringVal str1, StringVal str2] = if str1 == str2 then return TrueVal
                                       else return FalseVal
pEq [ArrayVal arr1, ArrayVal arr2] = if arr1 == arr2 then return TrueVal
                                     else return FalseVal
pEq [_, _] = return FalseVal
pEq _ = Left "'===' called with wrong number or type of arguments"

pLt :: Primitive
pLt [IntVal int1, IntVal int2] = if int1 < int2 then return TrueVal
                                 else return FalseVal
pLt [StringVal str1, StringVal str2] = if str1 < str2 then return TrueVal
                                       else return FalseVal
pLt _ = Left "'<' called with wrong number or type of arguments"

pAdd :: Primitive
pAdd [IntVal int1, IntVal int2] = return $ IntVal (int1 + int2)
pAdd [StringVal str1, StringVal str2] = return $ StringVal (str1 ++ str2)
pAdd [IntVal int, StringVal str] = return $ StringVal (show int ++ str)
pAdd [StringVal str, IntVal int] = return $ StringVal (str ++ show int)
pAdd _ = Left "'+' called with wrong number or type of arguments"

pMul :: Primitive
pMul [IntVal int1, IntVal int2] = return $ IntVal (int1 * int2)
pMul _ = Left "'*' called with wrong number or type of arguments"

pSub :: Primitive
pSub [IntVal int1, IntVal int2] = return $ IntVal (int1 - int2)
pSub _ = Left "'-' called with wrong number or type of arguments"

pMod :: Primitive
pMod [IntVal int1, IntVal int2] = return $ IntVal (int1 `mod` int2)
pMod _ = Left "'%' called with wrong number or type of arguments"

mkArray :: Primitive
mkArray [IntVal n] | n >= 0 = return $ ArrayVal (replicate n UndefinedVal)
mkArray _ = Left "Array() called with wrong number or type of arguments"

modifyEnv :: (Env -> Env) -> SubsM ()
modifyEnv f = SubsM $ \(env, _) -> Right ((), f env)

putVar :: Ident -> Value -> SubsM ()
putVar name val = modifyEnv $ Map.insert name val

getVar :: Ident -> SubsM Value
getVar name = SubsM $ \(env, _) -> case Map.lookup name env of
                                    Just val -> Right (val, env)
                                    Nothing -> Left "Variable not in scope"

getFunction :: FunName -> SubsM Primitive
getFunction name = SubsM $ \(env, pEnv) -> case Map.lookup name pEnv of
                                            Just func -> Right (func, env)
                                            Nothing -> Left
                                                       "Function not in scope"

-- Helper function to check if Value type is ArrayVal
isValArray :: Value -> Bool
isValArray (ArrayVal _) = True
isValArray _ = False

-- Helper function to extract the [Values] from ArrayVal
getArrayValues :: Value -> [Value]
getArrayValues (ArrayVal vs) = vs
getArrayValues _ = []

evalComp :: Value -> Ident -> ArrayCompr -> SubsM Value
evalComp (StringVal str) name comp = evalComp
                                     (ArrayVal [StringVal [a] | a <- str])
                                     name comp
evalComp (ArrayVal []) _ _ = return $ ArrayVal []
evalComp (ArrayVal (x:xs)) name comp = do
                                      -- Sets ident value to head of Array
                                      putVar name x
                                      v <- evalExpr (Compr comp)
                                      -- Eval recursively on tail of Array
                                      ArrayVal vs <- evalComp (ArrayVal xs)
                                                              name comp
                                      -- Checks if v is ArrayVal type;
                                      -- if yes, flattens list
                                      if isValArray v
                                      then return $
                                            ArrayVal (getArrayValues v ++ vs)
                                      else return $ ArrayVal (v : vs)

evalComp _ _ _ = fail "Syntax error."


evalExpr :: Expr -> SubsM Value
evalExpr (Number num) = return $ IntVal num
evalExpr (String str) = return $ StringVal str
evalExpr (Array exps) = do
                        values <- mapM evalExpr exps
                        return $ ArrayVal values
evalExpr Undefined = return UndefinedVal
evalExpr TrueConst = return TrueVal
evalExpr FalseConst = return FalseVal
evalExpr (Var name) = getVar name
evalExpr (Compr (ACBody expr)) = evalExpr expr
evalExpr (Compr (ACFor name expr comp)) = do
                                        -- Checks if varname already exists
                                        -- and saves it incase it's overwritten
                                        tempName <- SubsM $ \(env, _) ->
                                            case Map.lookup name env of
                                              (Just v) -> Right (Just v, env)
                                              Nothing -> Right (Nothing, env)
                                        i <- evalExpr expr
                                        v <- evalComp i name comp

                                        case tempName of
                                          (Just v') -> putVar name v'
                                          Nothing -> modifyEnv $
                                                     Map.delete name
                                        return v
evalExpr (Compr (ACIf expr comp)) = do
                                    eval_exp <- evalExpr expr
                                    if eval_exp == TrueVal
                                      -- Evaluates Compr if condition met
                                      then evalExpr (Compr comp)
                                    else if eval_exp == FalseVal
                                      -- Else returns empty list
                                      then return (ArrayVal [])
                                    else fail "Invalid if expression"
evalExpr (Call funcName exps) = do
                              (ArrayVal vs) <- evalExpr (Array exps)
                              func <- getFunction funcName
                              case func vs of
                                (Left e) -> fail e
                                (Right v) -> return v
evalExpr (Assign name expr) = do
                              v <- evalExpr expr
                              putVar name v
                              return v
evalExpr (Comma e1 e2) = do
                          _ <- evalExpr e1
                          evalExpr e2

runExpr :: Expr -> Either Error Value
runExpr expr = case runSubsM (evalExpr expr) initialContext of
                Left e -> Left e
                Right (v, _) -> Right v
