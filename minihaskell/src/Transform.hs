{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

-- module Transform (transform) where

import Types

-- TODO: Implement this function



-- transform :: (FExpr, [FDefinition]) -> (String, IExpr, [FExpr])
transform :: FProgram -> IProgram
transform (expr, defs) = iexpr : idefs ++ actuals
    where   
        fcalls = countFCalls (expr, defs)
        iexpr = ("result", frst (transformExpr (expr,fcalls,[])))
        idefs = map (\x -> transformDef x fcalls) defs
        vars = findFArgs (head defs)
        -- actuals = [transformActuals (thrd (transformExpr (expr,fcalls,[]))) vars fcalls]

-- -- for every function call of f, take the variable x of f(x) and add it to the list of actuals with the fcall expression
-- transformActuals :: [FExpr] -> [String] ->[(String,Int)] -> [IDefinition]
-- transformActuals [] _ _ = []
-- transformActuals (x:xs) (v:vs) fcalls = (v, IActuals (frst (transformExpr (x,fcalls,[])) : scnd (transformActuals xs (v:vs) fcalls))) : transformActuals xs vs fcalls

-- transformActualss :: [FExpr] -> [String] ->[(String,Int)] -> [IDefinition]
-- transformActualss [] _ _ = []
-- transformActualss (x:xs) (v:vs) (f:fcalls) = if (v, IActuals (frst (transformExpr (x,fcalls,[])) : scnd (transformActualss xs (v:vs) fcalls))) : transformActualss xs vs fcalls



-- transformActuals :: [FExpr] -> [String] ->[(String,Int)] -> IDefinition
-- transformActuals [] _ _ = ("", IActuals [])
-- transformActuals (x:xs) (v:vs) fcalls = (v, IActuals (frst (transformExpr (x,fcalls,[])) : frst (transformActuals xs vars fcalls)))


-----------------------------------------------------------------------------------------
------------------------------- Convert FExpr to IExpr ----------------------------------
-----------------------------------------------------------------------------------------

findFArgs :: FDefinition -> [(String)]
findFArgs (f, args, expr) = args


-----------------------------------------------------------------------------------------
------------------------------- Convert FExpr to IExpr ----------------------------------
-----------------------------------------------------------------------------------------

frst :: (a, b, c) -> a
frst (a,_,_) = a

scnd :: (a, b, c) -> b
scnd (_,b,_) = b

thrd :: (a, b, c) -> c
thrd (_,_,c) = c

transformExpr :: (FExpr,[(String, Int)],[FExpr]) -> (IExpr, [(String, Int)],[FExpr])
transformExpr (FVar s, occurs, factuals) = (IVar s, occurs, factuals)
transformExpr (FNum n, occurs, factuals) = (INum n, occurs, factuals)
transformExpr (FBool b, occurs, factuals) = (IBool b, occurs, factuals)
-- transformExpr (FParens b, occurs, factuals) = (IParens (fst (transformExpr (b, occurs, factuals))), occurs, factuals)
-- transformExpr (FIfThenElse i1 i2 i3, occurs, factuals) = (IIfThenElse (fst (transformExpr (i1, occurs, factuals))) (fst (transformExpr (i2, occurs, factuals))) (fst (transformExpr (i3, occurs, factuals))), occurs, factuals)
transformExpr (FCall f p, occurs, factuals) = (ICall (occurence f occurs) f, delete_occurence f occurs, nfactuals)
        where
        delete_occurence :: String -> [(String,Int)] -> [(String,Int)]
        delete_occurence _ [] = []
        delete_occurence f ((s,n):xs) = if f == s then xs else (s,n) : delete_occurence f xs
        add_nactuals :: [FExpr] -> [FExpr] -> [FExpr]
        add_nactuals [] ys = ys
        add_nactuals (x:xs) ys = add_nactuals xs (ys ++ [x])
        nfactuals = add_nactuals p factuals
-- transformExpr (FCompOp op i1 i2, occurs, factuals) = (ICompOp op (fst (transformExpr (i1, occurs, factuals))) (fst (transformExpr (i2, occurs, factuals))), occurs, factuals)
transformExpr (FBinaryOp op i1 i2, occurs, factuals) = (IBinaryOp op (frst a) (frst b), scnd b, thrd b)
        where         
        a = (transformExpr (i1, occurs, factuals))
        b = (transformExpr (i2, scnd a, thrd a))
-- transformExpr (FBooleanOp op i1 i2, occurs, factuals) = (IBooleanOp op (fst (transformExpr (i1, occurs, factuals))) (fst (transformExpr (i2, occurs, factuals))), occurs, factuals)

-----------------------------------------------------------------------------------------
------------------------------- Convert FDefinition to IDefinition -----------------------
-----------------------------------------------------------------------------------------
-- transformExpr (FVar s, occurs) = (IVar s, occurs)
-- transformExpr (FNum n, occurs) = (INum n, occurs)
-- transformExpr (FBool b, occurs) = (IBool b, occurs)
-- transformExpr (FParens b, occurs) = (IParens (fst (transformExpr (b, occurs))), occurs)
-- transformExpr (FIfThenElse i1 i2 i3, occurs) = (IIfThenElse (fst (transformExpr (i1, occurs))) (fst (transformExpr (i2, occurs))) (fst (transformExpr (i3, occurs))), occurs)
-- transformExpr (FCall f p, occurs) = (ICall (occurence f occurs) f, delete_occurence f occurs)
--         where
--         delete_occurence :: String -> [(String,Int)] -> [(String,Int)]
--         delete_occurence _ [] = []
--         delete_occurence f ((s,n):xs) = if f == s then xs else (s,n) : delete_occurence f xs
-- transformExpr (FCompOp op i1 i2, occurs) = (ICompOp op (fst (transformExpr (i1, occurs))) (fst (transformExpr (i2, occurs))), occurs)
-- transformExpr (FBinaryOp op i1 i2, occurs) = (IBinaryOp op (fst a) (fst b), snd b)
--         where a = (transformExpr (i1, occurs))
--               b = (transformExpr (i2, snd a))
-- transformExpr (FBooleanOp op i1 i2, occurs) = (IBooleanOp op (fst (transformExpr (i1, occurs))) (fst (transformExpr (i2, occurs))), occurs)
-- transformExpr (FUnaryOp op i, occurs) = (IUnaryOp op (fst (transformExpr (i, occurs))), occurs)



transformDef :: FDefinition -> [(String, Int)] -> IDefinition
transformDef (name, params, expr) lookup = (name, frst (transformExpr (expr,lookup,[])))

-----------------------------------------------------------------------------------------
-------------------------------- Count FCalls - step 1 ----------------------------------  
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

-----------------------------------------------------------------------------------------
-------------------------- FProgram -> IProgram - step 2 -------------------------------- 
-----------------------------------------------------------------------------------------

-- Take the result of countFCalls and use it to transform the program
-- into an IProgram. You can use the functions transformExpr and
-- transformDef from above.