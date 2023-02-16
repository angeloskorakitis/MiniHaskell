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
    Just funcDef -> fst (evalExpr env funcDef defs)
    Nothing -> error ("Function '" ++ funcName ++ "' is undefined")


-- Evaluate an Intensional expression given a function name and arguments
evalActuals :: IEnv -> [IDefinition] -> [IExpr] -> (IExpr,IEnv)
evalActuals env defs args  = evalExpr  (tail env) (args !! head env) defs


-- Evaluate an Intensional expression given a environment, expression, and definitions.
evalExpr :: IEnv -> IExpr -> [IDefinition] -> (IExpr,IEnv)
evalExpr env expr defs = case expr of
    IVar var -> evalVar env var defs
    INum num -> (INum num, env)
    IBool bool -> (IBool bool, env)
    IParens expr -> evalExpr env expr defs
    IIfThenElse cond thenExpr elseExpr -> evalIfThenElse env defs cond thenExpr elseExpr
    ICall i funcName -> evalCall env i funcName defs
    IActuals args -> evalActuals env defs args
    IBinaryOp  op expr1 expr2 -> evalBinaryOp env defs op expr1 expr2
    ICompOp op expr1 expr2 -> evalCompareOp env defs op expr1 expr2
    IBooleanOp op expr1 expr2 -> evalBooleanOp env defs op expr1 expr2
    IUnaryOp op expr -> evalUnaryOp env defs op expr


-- Evaluate a variable given an environment, variable name, and definitions.
-- Essentially, this function looks up the variable in the definitions and evaluates the expression.
evalVar :: IEnv -> String -> [IDefinition] -> (IExpr,IEnv)
evalVar env var defs = case lookup var defs of
    Just funcDef -> evalExpr env funcDef defs


-- Evaluates IfThenElse expression given an environment, definitions, condition, 'then' expression, and 'else' expression.
evalIfThenElse :: IEnv -> [IDefinition] ->  IExpr -> IExpr -> IExpr -> (IExpr,IEnv)
evalIfThenElse env defs cond thenExpr elseExpr =
    case fst (evalExpr env cond defs) of
        IBool True -> evalExpr env thenExpr defs
        IBool False -> evalExpr env elseExpr defs

-- Evaluates a function call given an environment, function call index, function name, and definitions.
-- Essentially, this function looks up the function in the definitions, adds the index at the head of the env and evaluates the expression.
evalCall :: IEnv -> Int -> String -> [IDefinition] -> (IExpr,IEnv)
evalCall env index function defs  = case lookup function defs of
    Just funcDef -> evalExpr (index:env) funcDef defs


-- Evaluate the compare operator given an environment, definitions, operator, and two expressions.
evalCompareOp :: IEnv -> [IDefinition] -> OpCompare -> IExpr -> IExpr -> (IExpr, IEnv)
evalCompareOp env defs op (INum n1) (INum n2) =
    case op of
        LtEq -> (IBool (n1 <= n2), env)
        Lt -> (IBool (n1 < n2), env)
        GtEq -> (IBool (n1 >= n2), env)
        Gt -> (IBool (n1 > n2), env)
        Eq -> (IBool (n1 == n2), env)
        Neq -> (IBool (n1 /= n2), env)
evalCompareOp env defs op n1 n2 = evalCompareOp env defs op eval_expr1 eval_expr2
    where
    eval_expr1 = fst (evalExpr env n1 defs)
    eval_expr2 = fst (evalExpr env n2 defs)


-- Evaluate the binary operator given an environment, definitions, operator, and expression.
evalBinaryOp :: IEnv -> [IDefinition] -> OpBinary -> IExpr -> IExpr -> (IExpr, IEnv)
evalBinaryOp env defs op (INum n1) (INum n2) =
    case op of
        Plus -> (INum (n1 + n2), env)
        Minus -> (INum (n1 - n2), env)
        Mult -> (INum (n1 * n2), env)
        Div -> (INum (n1 `div` n2), env)
evalBinaryOp env defs op n1 n2 = evalBinaryOp env defs op eval_expr1 eval_expr2
    where
    eval_expr1 = fst (evalExpr env n1 defs)
    eval_expr2 = fst (evalExpr env n2 defs)


-- Evaluate the boolean operator given an environment, definitions, operator, and expression.
evalBooleanOp :: IEnv -> [IDefinition] -> OpBool -> IExpr -> IExpr -> (IExpr, IEnv)
evalBooleanOp env defs op (IBool n1) (IBool n2) =
    case op of
        And -> (IBool (n1 && n2), env)
        Or -> (IBool (n1 || n2), env)
evalBooleanOp env defs op n1 n2 = evalBooleanOp env defs op eval_expr1 eval_expr2
    where
    eval_expr1 = fst (evalExpr env n1 defs)
    eval_expr2 = fst (evalExpr env n2 defs)


-- Evaluate the unary operator given an environment, definitions, operator, and expression.
evalUnaryOp :: IEnv -> [IDefinition] -> OpUnary -> IExpr -> (IExpr, IEnv)
evalUnaryOp env defs op (INum n) =
    case op of
        Positive -> (INum n, env)
        Negative -> (INum (-n), env)
evalUnaryOp env defs op (IBool n) =
    case op of
        Not -> (IBool (not n), env)
evalUnaryOp env defs op n = evalUnaryOp env defs op eval_expr
    where
    eval_expr = fst (evalExpr env n defs)










