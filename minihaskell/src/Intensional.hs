module Intensional (eval) where

import Types

-- TODO: Implement this function
-- Hint: The valid return values are INum and IBool
eval :: IProgram -> IExpr
eval iprogram = result
        where result = evalIDef [] "result" iprogram

-- Evaluate an Intensional expression given a function name and arguments
evalIDef :: IEnv -> String -> [IDefinition] -> IExpr
evalIDef env funcName defs =
  case lookup funcName defs of
    Just funcDef -> evalExpr env funcDef defs
    Nothing -> error ("Function '" ++ funcName ++ "' is undefined")

-- Evaluate an Intensional expression given a function name and arguments
evalActuals :: IEnv -> [IExpr] -> IExpr
evalActuals env args = (head args)

evalExpr :: IEnv -> IExpr -> [IDefinition] -> IExpr
evalExpr env expr defs = case expr of
    IVar var -> evalVar env var defs
    INum num -> INum num
    IBool bool -> IBool bool
    IParens expr -> evalExpr env expr defs
    IIfThenElse cond thenExpr elseExpr -> evalIfThenElse env cond thenExpr elseExpr defs
    ICall i funcName -> evalCall env i funcName defs
    IActuals args -> (evalActuals env args)
    ICompOp op expr1 expr2 -> evalCompareOp op (evalExpr env expr1 defs) (evalExpr env expr2 defs)
    IBinaryOp op expr1 expr2 -> evalBinaryOp op (evalExpr env expr1 defs) (evalExpr env expr2 defs)
    IBooleanOp op expr1 expr2 -> evalBooleanOp op (evalExpr env expr1 defs) (evalExpr env expr2 defs)

evalVar :: IEnv -> String -> [IDefinition] -> IExpr
evalVar env var defs = case lookup var defs of
    Just funcDef -> evalExpr env funcDef defs
    Nothing -> error ("Variable '" ++ var ++ "' is undefined")

evalIfThenElse :: IEnv -> IExpr -> IExpr -> IExpr -> [IDefinition] -> IExpr
evalIfThenElse env cond thenExpr elseExpr defs =
    case evalExpr env cond defs of
        IBool True -> evalExpr env thenExpr defs
        IBool False -> evalExpr env elseExpr defs
        _ -> error "Condition is not a boolean value"

evalCall :: IEnv -> Int -> String -> [IDefinition] -> IExpr
evalCall env i funcName defs  = case lookup funcName defs of
    Just funcDef -> evalExpr env funcDef defs

evalCompareOp :: OpCompare -> IExpr -> IExpr -> IExpr
evalCompareOp op (INum n1) (INum n2) =
    case op of
        LtEq -> IBool (n1 <= n2)
        Lt -> IBool (n1 < n2)
        GtEq -> IBool (n1 >= n2)
        Gt -> IBool (n1 > n2)
        Eq -> IBool (n1 == n2)
        Neq -> IBool (n1 /= n2)


evalBinaryOp :: OpBinary -> IExpr -> IExpr -> IExpr
evalBinaryOp op (INum n1) (INum n2) =
    case op of
        Plus -> INum (n1 + n2)
        Mult -> INum (n1 * n2)
        Minus -> INum (n1 - n2)
        Div -> INum (n1 `div` n2)

evalBooleanOp :: OpBool -> IExpr -> IExpr -> IExpr
evalBooleanOp op (IBool b1) (IBool b2) =
    case op of
        And -> IBool (b1 && b2)
        Or -> IBool (b1 || b2)

evalUnaryOp :: OpUnary -> IExpr -> IExpr
evalUnaryOp op (INum n) =
    case op of
        Positive -> INum n
        Negative -> INum (-n)










