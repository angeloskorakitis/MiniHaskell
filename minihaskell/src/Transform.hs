module Transform (transform) where

import Types
import Data.List ( groupBy, sortBy )
import Data.Ord (comparing)


transform :: FProgram -> IProgram
transform (expr, defs) =  iprogram
  where
    fcalls = findFunctions defs                                                      -- Find all the function calls in the program.
    transformed_expr = transformExpr (expr, fcalls, [])                              -- Transform the result expression to IExpr.
    iexpr = ("result", (\(iexpr,_,_) -> iexpr) transformed_expr)                     -- Get the IExpr from the tuple.
    idefs = transformDefs defs ((\(_,fcalls,_) -> fcalls) transformed_expr)          -- Transform the definitions to IDefinitions.
    function_args =  (\(_,_,factuals) -> factuals) transformed_expr ++ concat ((\(_,_,factuals) -> factuals) idefs) -- Get the function arguments from the result expression and the definitions.
    iactuals =groupActuals (transformActuals function_args defs)                     -- Transform the actuals to IProgram, change the function name to the parameters of the function.
    iprogram = iexpr : (\(iexprs,_,_) -> iexprs) idefs ++ iactuals                   -- The final IProgram.


-- Combines the IExprs of the same function parameter, i.e. same String
groupActuals :: [(String, [IExpr])] -> [(String, IExpr)]
groupActuals [] = []
groupActuals ((s, iexprs):xs) = (s,IActuals (iexprs ++ concatMap snd matchingTuples)) : groupActuals nonMatchingTuples
  where
    matchingTuples = filter (\(s', _) -> s == s') xs
    nonMatchingTuples = filter (\(s', _) -> s /= s') xs


-- Transforms the actuals to IProgram, given the definitions and the function name and intermediate expressions.
transformActuals :: [(String, [IExpr])] -> [FDefinition] -> [(String,[IExpr])]
transformActuals [] _ = []
transformActuals ((f, iexprs):xs) fdefs = findActuals name iexprs ++ transformActuals xs fdefs
        where
                fargs = findFParameters fdefs f
                name = if null fargs then [f] else fargs


-- Given the definitions of the functions and the function name it returns the parameters of the function.
findFParameters :: [FDefinition] -> String -> [String]
findFParameters [] _ = [] 
findFParameters ((s, params, _):xs) f = if f == s then params else findFParameters xs f


-- For each parameter of the function it creates a list of IExprs- Actuals.
findActuals :: [String] -> [IExpr] -> [(String, [IExpr])]
findActuals [] _ = []
findActuals (x:xs) [] = [(concat (x:xs), [])]
findActuals (x:xs) (y:ys) = (x, [y]) : findActuals xs ys


-----------------------------------------------------------------------------------------
------------------------- Transform Expressions to IExpressions -------------------------
-----------------------------------------------------------------------------------------


transformExpr :: (FExpr,[(String, Int)],[(String,[IExpr])]) -> (IExpr, [(String, Int)],[(String,[IExpr])])
transformExpr (FVar s, fcalls, factuals) = (IVar s, fcalls, factuals)
transformExpr (FNum n, fcalls, factuals) = (INum n, fcalls, factuals)
transformExpr (FBool b, fcalls, factuals) = (IBool b, fcalls, factuals)
transformExpr (FParens e, fcalls, factuals) = (IParens iexpr, fcalls', factuals')
        where
                transformed_expr = transformExpr (e, fcalls, factuals)
                iexpr = (\(iexpr,_,_) -> iexpr) transformed_expr
                fcalls' = (\(_,fcalls,_) -> fcalls) transformed_expr
                factuals' = (\(_,_,factuals) -> factuals) transformed_expr

-- For readability purposes we use frst, scnd, thrd instead of \(a,b,c) -> a, \(a,b,c) -> b, \(a,b,c) -> c...
transformExpr (FIfThenElse cond_expr then_expr else_expr, fcalls, factuals) = (IIfThenElse cond_iexpr then_iepxr else_iexpr, else_fcalls, else_factuals)
        where
                cond_transformed_expr = transformExpr (cond_expr, fcalls, factuals)
                cond_iexpr = frst cond_transformed_expr
                cond_fcalls = scnd cond_transformed_expr
                cond_factuals = thrd cond_transformed_expr
                then_transformed_expr = transformExpr (then_expr, cond_fcalls, cond_factuals)
                then_iepxr = frst then_transformed_expr
                then_fcalls = scnd then_transformed_expr
                then_factuals = thrd then_transformed_expr
                else_transformed_expr = transformExpr (else_expr, then_fcalls, then_factuals)
                else_iexpr = frst else_transformed_expr
                else_fcalls = scnd else_transformed_expr
                else_factuals = thrd else_transformed_expr

transformExpr (FCall f p, fcalls, factuals) = (ICall (occurence f fcalls) f, fcalls'', factuals'')
        where 
                fcalls' = increaseOccurence f fcalls
                factuals' = factuals ++ [(f,iexpr)]
                transform_expr = transformExprs p fcalls' factuals'
                iexpr = frst transform_expr
                fcalls'' = if null (scnd transform_expr) then fcalls' else last (scnd transform_expr)
                factuals'' = if null (thrd transform_expr) then factuals' else last (thrd transform_expr)

-----------------------------------------------------------------------------------------

transformExpr (FCompOp op left_expr right_expr, occurs, factuals) = (ICompOp op left_iexpr right_iexpr, right_iexpr_fcalls,  right_iexpr_factuals)
        where
                left_transformed_expr = transformExpr (left_expr, occurs, factuals)
                left_iexpr = frst left_transformed_expr
                left_iexpr_fcalls = scnd left_transformed_expr
                left_iexpr_factuals = thrd left_transformed_expr
                right_transformed_expr = transformExpr (right_expr, left_iexpr_fcalls, left_iexpr_factuals)
                right_iexpr = frst right_transformed_expr
                right_iexpr_fcalls = scnd right_transformed_expr
                right_iexpr_factuals = thrd right_transformed_expr
transformExpr (FBinaryOp op left_expr right_expr, occurs, factuals) = (IBinaryOp op left_iexpr right_iexpr, right_iexpr_fcalls,  right_iexpr_factuals)
        where
                left_transformed_expr = transformExpr (left_expr, occurs, factuals)
                left_iexpr = frst left_transformed_expr
                left_iexpr_fcalls = scnd left_transformed_expr
                left_iexpr_factuals = thrd left_transformed_expr
                right_transformed_expr = transformExpr (right_expr, left_iexpr_fcalls, left_iexpr_factuals)
                right_iexpr = frst right_transformed_expr
                right_iexpr_fcalls = scnd right_transformed_expr
                right_iexpr_factuals = thrd right_transformed_expr

transformExpr (FBooleanOp op left_expr right_expr, occurs, factuals) = (IBooleanOp op left_iexpr right_iexpr, right_iexpr_fcalls,  right_iexpr_factuals)
        where
                left_transformed_expr = transformExpr (left_expr, occurs, factuals)
                left_iexpr = frst left_transformed_expr
                left_iexpr_fcalls = scnd left_transformed_expr
                left_iexpr_factuals = thrd left_transformed_expr
                right_transformed_expr = transformExpr (right_expr, left_iexpr_fcalls, left_iexpr_factuals)
                right_iexpr = frst right_transformed_expr
                right_iexpr_fcalls = scnd right_transformed_expr
                right_iexpr_factuals = thrd right_transformed_expr

transformExpr (FUnaryOp op e, fcalls, factuals) = (IUnaryOp op iexpr, fcalls', factuals')
        where
                transformed_expr = transformExpr (e, fcalls, factuals)
                iexpr = (\(iexpr,_,_) -> iexpr) transformed_expr
                fcalls' = (\(_,fcalls,_) -> fcalls) transformed_expr
                factuals' = (\(_,_,factuals) -> factuals) transformed_expr



transformExprs :: [FExpr] -> [(String, Int)] -> [(String,[IExpr])] -> ([IExpr], [[(String, Int)]], [[(String,[IExpr])]])
transformExprs [] _ _ = ([],[],[])
transformExprs (x:xs) occurs factuals = (frst transformed_expr : frst transformed_exprs,  scnd transformed_expr : scnd transformed_exprs, thrd transformed_expr : thrd transformed_exprs)
        where
                transformed_expr = transformExpr (x, occurs, factuals)
                transformed_exprs = transformExprs xs (scnd transformed_expr) (thrd transformed_expr)


----------------------------------------------------------------------------------------------
------------------------------- Convert FDefinition to IDefinition ---------------------------
----------------------------------------------------------------------------------------------


transformDef :: FDefinition -> [(String, Int)] -> (IDefinition, [(String,Int)], [(String,[IExpr])])
transformDef (name, _, expr) lookup = ((name, iexpr), fcalls, factuals)
        where 
        transformed_expr = transformExpr (expr,lookup,[])
        iexpr = (\(iexpr,_,_) -> iexpr) transformed_expr
        fcalls = (\(_,fcalls',_) -> fcalls') transformed_expr
        factuals = (\(_,_,factuals') -> factuals') transformed_expr


transformDefs :: [FDefinition] -> [(String, Int)] -> ([IDefinition], [[(String,Int)]], [[(String,[IExpr])]])
transformDefs [] _ = ([],[],[])
transformDefs (x:xs) lookup = (frst transformed_def : frst transformed_defs , scnd transformed_def : scnd transformed_defs, thrd transformed_def : thrd transformed_defs)
        where 
        transformed_def = transformDef x lookup
        transformed_defs = transformDefs xs lookup


----------------------------------------------------------------------------------------------
------------------------------------ Helper functions ----------------------------------------
----------------------------------------------------------------------------------------------


-- Returns the number of occurences of a function.
occurence :: String -> [(String,Int)] -> Int
occurence f [] = 0
occurence f ((s,n):xs) = if f == s then n else occurence f xs


-- Increases the number of occurences of a function by 1.
increaseOccurence :: String -> [(String,Int)] -> [(String,Int)]
increaseOccurence _ [] = []
increaseOccurence f ((s,n):xs) = if f == s then (s,n+1) : xs else (s,n) : increaseOccurence f xs


-- Find all functions in a list of definitions. Returns a tuple with the name of the function and the number 0, i.e. the first occurence of the function.
findFunctions :: [FDefinition] -> [(String,Int)]
findFunctions [] = []
findFunctions ((name,_,_):xs) = (name,0) : findFunctions xs


frst :: (a, b, c) -> a
frst (a,_,_) = a

scnd :: (a, b, c) -> b
scnd (_,b,_) = b

thrd :: (a, b, c) -> c
thrd (_,_,c) = c