{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

-- module Transform (transform) where

import Types
import Data.List (groupBy)
import System.Info (arch)


-- TODO: Implement this function



-- transform :: (FExpr, [FDefinition]) -> (String, IExpr, [FExpr])
-- ( thrd (transformExpr (expr,fcalls,[])) ++ concat (scnd idefs))
-- iexpr : (frst idefs) ++ actuals
-- transform :: FProgram -> IProgram
transform (expr, defs) = iexpr : (frst idefs) ++ actuals
  where
    fcalls = countFCalls (expr, defs)
    transformedExpr = transformExpr (expr, fcalls, [])
    iexpr = ("result", frst transformedExpr)
    idefs = transformDefs defs (scnd transformedExpr)
    iprogramTemp = iexpr : frst idefs
    functionArgs = flattenFExprArgs (thrd transformedExpr ++ concat (scnd idefs))
    actuals = transformActualsToIProgram (transformActs (transformActuals functionArgs defs (last (thrd idefs))))
-----------------------------------------------------------------------------------------

-- Flattens the arguments of the function calls.  
-- e.g. [("f",[(FCall "f" [FCall "f" [FNum 4]]])] ->  [("f",[(FCall "f" [FCall "f" [FNum 4]]]),("f",[FCall "f" [FNum 4]])]
flattenFExprArgs :: [(String,[FExpr])] -> [(String,[FExpr])]
flattenFExprArgs [] = []
flattenFExprArgs ((f,[FCall name args]):xs) = if (any isFCall args) then [(f,[FCall name args]),(f,args)] ++ (flattenFExprArgs ((f,args):xs)) else (f, args) : (flattenFExprArgs xs)
        where
        isFCall :: FExpr -> Bool
        isFCall (FCall _ _) = True
        isFCall _ = False
flattenFExprArgs other = other




transformActualsToIProgram :: [(String, [IExpr])] -> IProgram
transformActualsToIProgram = map groupToIDef . groupBy (\x y -> fst x == fst y)
  where
    groupToIDef g = (fst (head g), IActuals (concatMap snd g))


transformActs :: [[(String,IExpr)]] -> [(String,[IExpr])]
transformActs acts = foldl combine [] acts
  where
    combine acc xs = acc ++ map (\(s,e) -> (s, [e])) xs

transformActuals :: [(String, [FExpr])] -> [FDefinition] -> [(String, Int)] -> [[(String,IExpr)]]
transformActuals [] _  _ = []
transformActuals ((s, e):xs) fdefs fcalls = findActuals (findFArgs fdefs  s) e fcalls : transformActuals xs fdefs fcalls
        where 
        findFArgs :: [FDefinition] -> String -> [String]
        findFArgs [] _ = []
        findFArgs ((s, args, _):xs) f = if f == s then args else findFArgs xs f

findActuals :: [String] -> [FExpr] -> [(String, Int)] -> [(String,IExpr)]
findActuals [] [] _ = []
findActuals (x:xs) (y:ys) fcalls = (x, (\(iexpr,_,_) -> iexpr) transformed_expr) : findActuals xs ys ((\(_,occurs,_) -> occurs) transformed_expr)
  where
    transformed_expr = transformExpr (y, fcalls, [])
-- ΕΔΩ ΝΑ ΠΡΟΣΕΞΩ!!! ΘΕΛΕΙ ΒΕΛΤΙΩΣΗ
    




-- -----------------------------------------------------------------------------------------
-- ------------------------------- Convert FExpr to IExpr ----------------------------------
-- -----------------------------------------------------------------------------------------

frst :: (a, b, c) -> a
frst (a,_,_) = a

scnd :: (a, b, c) -> b
scnd (_,b,_) = b

thrd :: (a, b, c) -> c
thrd (_,_,c) = c

transformExpr :: (FExpr,[(String, Int)],[(String,[FExpr])]) -> (IExpr, [(String, Int)],[(String,[FExpr])])
transformExpr (FVar s, occurs, factuals) = (IVar s, occurs, factuals)
transformExpr (FNum n, occurs, factuals) = (INum n, occurs, factuals)
transformExpr (FBool b, occurs, factuals) = (IBool b, occurs, factuals)
transformExpr (FParens e, occurs, factuals) = (IParens (frst a), scnd a, thrd a)
        where
                a = (transformExpr (e, occurs, factuals))

-- transformExpr (FIfThenElse i1 i2 i3, occurs, factuals) = (IIfThenElse (fst (transformExpr (i1, occurs, factuals))) (fst (transformExpr (i2, occurs, factuals))) (fst (transformExpr (i3, occurs, factuals))), occurs, factuals)
transformExpr (FCall f p, occurs, factuals) = (ICall (occurence f occurs) f, delete_occurence f occurs, factuals ++ [(f, p)])
        where
        delete_occurence :: String -> [(String,Int)] -> [(String,Int)]
        delete_occurence _ [] = []
        delete_occurence f ((s,n):xs) = if f == s then xs else (s,n) : delete_occurence f xs
-- transformExpr (FCompOp op i1 i2, occurs, factuals) = (ICompOp op (fst (transformExpr (i1, occurs, factuals))) (fst (transformExpr (i2, occurs, factuals))), occurs, factuals)
transformExpr (FBinaryOp op i1 i2, occurs, factuals) = (IBinaryOp op (frst a) (frst b), scnd b, thrd b)
        where         
        a = (transformExpr (i1, occurs, factuals))
        b = (transformExpr (i2, scnd a, thrd a))
-- transformExpr (FBooleanOp op i1 i2, occurs, factuals) = (IBooleanOp op (fst (transformExpr (i1, occurs, factuals))) (fst (transformExpr (i2, occurs, factuals))), occurs, factuals)

-----------------------------------------------------------------------------------------
------------------------------- Convert FDefinition to IDefinition -----------------------
-----------------------------------------------------------------------------------------

transformDef :: FDefinition -> [(String, Int)] -> (IDefinition,[(String,[FExpr])], [(String,Int)])
transformDef (name, params, expr) lookup = ((name, frst transformed), thrd transformed, scnd transformed)
        where transformed = transformExpr (expr,lookup,[])

transformDefs :: [FDefinition] -> [(String, Int)] -> ([IDefinition], [[(String,[FExpr])]], [[(String,Int)]])
transformDefs [] _ = ([],[],[])
transformDefs (x:xs) lookup = (frst transformed_def : frst transformed_defs , scnd transformed_def : scnd transformed_defs, (thrd transformed_def : thrd transformed_defs))
        where 
        transformed_def = transformDef x lookup
        transformed_defs = transformDefs xs lookup



-----------------------------------------------------------------------------------------
--------------------------- Convert FCalls to [FCalls] ----------------------------------
-----------------------------------------------------------------------------------------

-- flattenFCalls :: FExpr -> [FExpr]
-- flattenFCalls (FCall f [FNum s]) = [FCall f [FNum s]]
-- flattenFCalls (FCall f [FBool s]) = [FCall f [FBool s]]
-- flattenFCalls (FCall f [FVar s]) = [FCall f [FVar s]]
-- flattenFCalls (FCall f [FParens s]) = [FCall f [FParens s]]
-- flattenFCalls (FCall f [FCall s p]) = [FCall f [FCall s p]]
-- flattenFCalls (FCall f [FBinaryOp op s1 s2]) = [FCall f [FBinaryOp op s1 s2]]
-- flattenFCalls (FCall f [FIfThenElse s1 s2 s3]) = [FCall f [FIfThenElse s1 s2 s3]]
-- flattenFCalls (FCall f [FCompOp op s1 s2]) = [FCall f [FCompOp op s1 s2]]
-- flattenFCalls (FCall f [FBooleanOp op s1 s2]) = [FCall f [FBooleanOp op s1 s2]]

-- flattenFCalls (FCall f [arg]) = FCall f [] : flattenFCalls arg


-- flattenFProgram :: FProgram -> [FExpr]
-- flattenFProgram (expr, defs) = flattenFCalls expr ++ concatMap flattenFDef defs

-- flattenFDef :: FDefinition -> [FExpr]
-- flattenFDef (name, params, expr) = flattenFCalls expr


-----------------------------------------------------------------------------------------
----------------------------- Count Function Calls --------------------------------------
-----------------------------------------------------------------------------------------

countFCalls :: FProgram -> [(String,Int)]
countFCalls (expr, defs) =  expr_fcalls ++ defs_fcalls
        where 
        expr_fcalls = countFCallsExpr expr []
        defs_fcalls = countFCallsDefs defs expr_fcalls   


countFCallsExpr :: FExpr -> [(String,Int)] ->[(String,Int)]
countFCallsExpr (FVar s) _ = []
countFCallsExpr (FNum n) _ = []
countFCallsExpr (FBool b) _ = []
countFCallsExpr (FParens b) lookup = countFCallsExpr b lookup
countFCallsExpr (FIfThenElse i1 i2 i3) lookup = op1 ++ op2 ++ op3
        where   op1 = (countFCallsExpr i1 lookup)
                op2 = (countFCallsExpr i2 op1)
                op3 = (countFCallsExpr i3 op2)

countFCallsExpr (FCall f p) lookup = (countFCallsExprs p nlookup) ++ ([(f, (occurence f nlookup))])
        where nlookup = (increase_occurence f lookup)

countFCallsExpr (FCompOp op i1 i2) lookup = op1 ++ op2
        where   op1 = (countFCallsExpr i1 lookup)
                op2 = (countFCallsExpr i2 op1)
countFCallsExpr (FBinaryOp op i1 i2) lookup = op1 ++ op2
        where   op1 = (countFCallsExpr i1 lookup)
                op2 = (countFCallsExpr i2 op1)
countFCallsExpr (FBooleanOp op i1 i2) lookup = op1 ++ op2
        where   op1 = (countFCallsExpr i1 lookup)
                op2 = (countFCallsExpr i2 op1)
countFCallsExpr (FUnaryOp op i) lookup = countFCallsExpr i lookup

countFCallsExprs :: [FExpr] -> [(String,Int)] ->[(String,Int)]
countFCallsExprs [] _ = []
countFCallsExprs (x:xs) lookup = a ++ (countFCallsExprs xs a)
        where a = (countFCallsExpr x lookup)

countFCallsDefs :: [FDefinition] -> [(String,Int)] -> [(String,Int)]
countFCallsDefs [] _ = []
countFCallsDefs ((f,_,expr):xs) lookup = a ++ (countFCallsDefs xs a)
        where a = (countFCallsExpr expr lookup)

increase_occurence :: String -> [(String,Int)] -> [(String,Int)]
increase_occurence f [] = [(f,0)]
increase_occurence f ((s,n):xs) = if f == s then (s,n+1):xs else (s,n):(increase_occurence f xs)

occurence :: String -> [(String,Int)] -> Int
occurence f [] = 0
occurence f ((s,n):xs) = if f == s then n else occurence f xs

