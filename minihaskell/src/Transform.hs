module Transform (transform) where

import Types
import Data.List ( groupBy, sortBy )
import Data.Ord (comparing)
-- import Data.Map as Map

transform :: FProgram -> IProgram
transform (expr, defs) = iexpr : frst idefs ++ iactuals
  where
    fcalls = findFunctions defs
    transformedExpr = transformExpr (expr, fcalls, [])
    iexpr = ("result", (\(iexpr,_,_) -> iexpr) transformedExpr)
    idefs = transformDefs defs (scnd transformedExpr)
    function_args =  (\(_,_,x) -> x) transformedExpr ++ concat (scnd idefs)
    actuals = groupActuals function_args
    iactuals = transformActualsToIProgram actuals defs


-- Combines the IExprs of the same function call, i.e. same String
groupActuals :: [(String, [IExpr])] -> [(String, [IExpr])]
groupActuals = map combine . groupBy sameString . sortBy sortString
  where
    sortString (s1, _) (s2, _) = compare s1 s2
    sameString (s1, _) (s2, _) = s1 == s2
    combine ((s, exprs):xs) = (s, concatMap snd ((s, exprs):xs))


transformActualsToIProgram :: [(String, [IExpr])] -> [FDefinition] -> [(String, IExpr)]
transformActualsToIProgram [] _ = []
transformActualsToIProgram ((s, e):xs) fdefs = findActuals (if null fargs then [s] else fargs) e ++ transformActualsToIProgram xs fdefs
        where
                fargs = findFArgs fdefs s


findFArgs :: [FDefinition] -> String -> [String]
findFArgs [] _ = [] 
findFArgs ((s, args, _):xs) f = if f == s then args else findFArgs xs f

findActuals :: [String] -> [IExpr] -> [(String, IExpr)]
findActuals [] _ = []
findActuals _ [] = []
findActuals (x1:x2:xs) (y1:y2:ys) = (x1, IActuals [y1]) : findActuals (x2:xs) (y2:ys)
findActuals (x:xs) ys = (x,IActuals ys) : findActuals xs []


transformExpr :: (FExpr,[(String, Int)],[(String,[IExpr])]) -> (IExpr, [(String, Int)],[(String,[IExpr])])
transformExpr (FVar s, fcalls, factuals) = (IVar s, fcalls, factuals)
transformExpr (FNum n, fcalls, factuals) = (INum n, fcalls, factuals)
transformExpr (FBool b, fcalls, factuals) = (IBool b, fcalls, factuals)
transformExpr (FParens e, fcalls, factuals) = (IParens iexpr, fcalls', factuals')
        where
                transformed_expr = transformExpr (e, fcalls, factuals)
                iexpr = (\(iexpr,_,_) -> iexpr) transformed_expr
                fcalls' = (\(_,fcalls',_) -> fcalls') transformed_expr
                factuals' = (\(_,_,factuals') -> factuals') transformed_expr

transformExpr (FIfThenElse e1 e2 e3, fcalls, factuals) = (IIfThenElse iexpr1 iexpr2 iexpr3, fcalls3, factuals3)
        where
                i1 = transformExpr (e1, fcalls, factuals)
                iexpr1 = frst i1
                fcalls1 = scnd i1
                factuals1 = thrd i1
                i2 = transformExpr (e2, fcalls1, factuals1)
                iexpr2 = frst i2
                fcalls2 = scnd i2
                factuals2 = thrd i2
                i3 = transformExpr (e3, fcalls2, factuals2)
                iexpr3 = frst i3
                fcalls3 = scnd i3
                factuals3 = thrd i3
transformExpr (FCall f p, fcalls, factuals) = (ICall (occurence f fcalls) f, fcalls'', factuals'')
        where 
                fcalls' = increaseOccurence f fcalls
                factuals' = factuals ++ [(f,iexpr)]
                transform_expr = transformExprs p fcalls' factuals'
                iexpr = frst transform_expr
                fcalls'' = if null (scnd transform_expr) then [] else last (scnd transform_expr)
                factuals'' = if null (thrd transform_expr) then [] else last (thrd transform_expr)

-----------------------------------------------------------------------------------------

transformExpr (FCompOp op e1 e2, occurs, factuals) = (ICompOp op (frst a) (frst b), scnd b, thrd b)
        where         
        a = transformExpr (e1, occurs, factuals)
        b = transformExpr (e2, scnd a, thrd a)
transformExpr (FBinaryOp op i1 i2, occurs, factuals) = (IBinaryOp op (frst a) (frst b), scnd b, thrd b)
        where         
        a = transformExpr (i1, occurs, factuals)
        b = transformExpr (i2, scnd a, thrd a)
transformExpr (FBooleanOp op i1 i2, occurs, factuals) = (IBooleanOp op (frst a) (frst b), scnd b, thrd b)
        where         
        a = transformExpr (i1, occurs, factuals)
        b = transformExpr (i2, scnd a, thrd a)
transformExpr (FUnaryOp op e, fcalls, factuals) = (IUnaryOp op iexpr, fcalls', factuals')
        where
                transformed_expr = transformExpr (e, fcalls, factuals)
                iexpr = (\(iexpr,_,_) -> iexpr) transformed_expr
                fcalls' = (\(_,fcalls',_) -> fcalls') transformed_expr
                factuals' = (\(_,_,factuals') -> factuals') transformed_expr


transformExprs :: [FExpr] -> [(String, Int)] -> [(String,[IExpr])] -> ([IExpr], [[(String, Int)]], [[(String,[IExpr])]])
transformExprs [] _ _ = ([],[],[])
transformExprs (x:xs) occurs factuals = (frst transformed_expr : frst transformed_exprs,  scnd transformed_expr : scnd transformed_exprs, thrd transformed_expr : thrd transformed_exprs)
        where
                transformed_expr = transformExpr (x, occurs, factuals)
                transformed_exprs = transformExprs xs (scnd transformed_expr) (thrd transformed_expr)



-----------------------------------------------------------------------------------------
------------------------------- Convert FDefinition to IDefinition ----------------------
-----------------------------------------------------------------------------------------

transformDef :: FDefinition -> [(String, Int)] -> (IDefinition,[(String,[IExpr])], [(String,Int)])
transformDef (name, _, expr) lookup = ((name, iexprs), thrd transformed_expr, fcalls)
        where 
        transformed_expr = transformExpr (expr,lookup,[])
        iexprs = frst transformed_expr
        fcalls = scnd transformed_expr


transformDefs :: [FDefinition] -> [(String, Int)] -> ([IDefinition], [[(String,[IExpr])]], [[(String,Int)]])
transformDefs [] _ = ([],[],[])
transformDefs (x:xs) lookup = (frst transformed_def : frst transformed_defs , scnd transformed_def : scnd transformed_defs, thrd transformed_def : thrd transformed_defs)
        where 
        transformed_def = transformDef x lookup
        transformed_defs = transformDefs xs lookup


-----------------------------------------------------------------------------------------
-------------------------------- Helper functions ---------------------------------------
-----------------------------------------------------------------------------------------


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