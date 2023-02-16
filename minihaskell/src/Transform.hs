{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

-- module Transform (transform) where

import Types
import Data.List ( groupBy, sortBy )
import Data.Ord (comparing)
-- import Data.Map as Map

transform :: FProgram -> IProgram
transform (expr, defs) = iexpr : (frst idefs) ++ iactuals
  where
    fcalls = findFunctions defs
    transformedExpr = transformExpr (expr, fcalls, [])
    iexpr = ("result", (\(x,_,_)->x) transformedExpr)
    idefs = transformDefs defs (scnd transformedExpr)
    function_args =  ((\(_,_,x) -> x) transformedExpr ++ concat (scnd idefs))
    actuals = groupActuals function_args
    iactuals = (transformActualsToIProgram actuals defs)


-----------------------------------------------------------------------------------------


-- Combines the IExprs of the same function call, i.e. same String
groupActuals :: [(String, [IExpr])] -> [(String, [IExpr])]
groupActuals = map combine . groupBy sameString . sortBy sortString
  where
    sortString (s1, _) (s2, _) = compare s1 s2
    sameString (s1, _) (s2, _) = s1 == s2
    combine ((s, exprs):xs) = (s, concatMap snd ((s, exprs):xs))


transformActualsToIProgram :: [(String, [IExpr])] -> [FDefinition] -> [(String, IExpr)]
transformActualsToIProgram [] _ = []
transformActualsToIProgram ((s, e):xs) fdefs = (findActuals (findFArgs fdefs s) e) ++ transformActualsToIProgram xs fdefs


findFArgs :: [FDefinition] -> String -> [String]
findFArgs [] _ = []
findFArgs ((s, args, _):xs) f = if f == s then args else findFArgs xs f

findActuals :: [String] -> [IExpr] -> [(String, IExpr)]
findActuals _ [] = []
findActuals (x1:x2:xs) (y1:y2:ys) = (x1, IActuals [y1]) : findActuals (x2:xs) (y2:ys)
findActuals (x:xs) ys = (x,IActuals ys) : findActuals xs []


transformExpr :: (FExpr,[(String, Int)],[(String,[IExpr])]) -> (IExpr, [(String, Int)],[(String,[IExpr])])
transformExpr (FVar s, occurs, factuals) = (IVar s, occurs, factuals )
transformExpr (FNum n, occurs, factuals) = (INum n, occurs, factuals)
transformExpr (FBool b, occurs, factuals) = (IBool b, occurs, factuals)
transformExpr (FParens e, occurs, factuals) = (IParens (frst a), scnd a, thrd a)
        where
                a = (transformExpr (e, occurs, factuals))

-- transformExpr (FIfThenElse i1 i2 i3, occurs, factuals) = (IIfThenElse (fst (transformExpr (i1, occurs, factuals))) (fst (transformExpr (i2, occurs, factuals))) (fst (transformExpr (i3, occurs, factuals))), occurs, factuals)
transformExpr (FCall f p, occurs, factuals) = (ICall (occurence f occurs) f, last b,last d)
        where 
                a = (transformExprs p (increaseOccurence f occurs) (factuals ++ [(f,c)]))
                b = (scnd a)
                c = (frst a)
                d = (thrd a)
                        

-- transformExpr (FCompOp op i1 i2, occurs, factuals) = (ICompOp op (fst (transformExpr (i1, occurs, factuals))) (fst (transformExpr (i2, occurs, factuals))), occurs, factuals)
transformExpr (FBinaryOp op i1 i2, occurs, factuals) = (IBinaryOp op (frst a) (frst b), scnd b, thrd b)
        where         
        a = (transformExpr (i1, occurs, factuals))
        b = (transformExpr (i2, scnd a, thrd a))
-- transformExpr (FBooleanOp op i1 i2, occurs, factuals) = (IBooleanOp op (fst (transformExpr (i1, occurs, factuals))) (fst (transformExpr (i2, occurs, factuals))), occurs, factuals)


transformExprs :: [FExpr] -> [(String, Int)] -> [(String,[IExpr])] -> ([IExpr], [[(String, Int)]], [[(String,[IExpr])]])
transformExprs [] _ _ = ([],[],[])
transformExprs (x:xs) occurs factuals = (frst transformed_expr : frst transformed_exprs,  (scnd transformed_expr : scnd transformed_exprs), (thrd transformed_expr : thrd transformed_exprs))
        where
                transformed_expr = transformExpr (x, occurs, factuals)
                transformed_exprs = transformExprs xs (scnd transformed_expr) (thrd transformed_expr)




-----------------------------------------------------------------------------------------
------------------------------- Convert FDefinition to IDefinition -----------------------
-----------------------------------------------------------------------------------------

transformDef :: FDefinition -> [(String, Int)] -> (IDefinition,[(String,[IExpr])], [(String,Int)])
transformDef (name, params, expr) lookup = ((name, frst transformed), thrd transformed, scnd transformed)
        where transformed = transformExpr (expr,lookup,[])

transformDefs :: [FDefinition] -> [(String, Int)] -> ([IDefinition], [[(String,[IExpr])]], [[(String,Int)]])
transformDefs [] _ = ([],[],[])
transformDefs (x:xs) lookup = (frst transformed_def : frst transformed_defs , scnd transformed_def : scnd transformed_defs, (thrd transformed_def : thrd transformed_defs))
        where 
        transformed_def = transformDef x lookup
        transformed_defs = transformDefs xs lookup


-----------------------------------------------------------------------------------------
-------------------------------- Helper functions ---------------------------------------
-----------------------------------------------------------------------------------------

occurence :: String -> [(String,Int)] -> Int
occurence f [] = 0
occurence f ((s,n):xs) = if f == s then n else occurence f xs

increaseOccurence :: String -> [(String,Int)] -> [(String,Int)]
increaseOccurence _ [] = []
increaseOccurence f ((s,n):xs) = if f == s then (s,n+1) : xs else (s,n) : increaseOccurence f xs

findFunctions :: [FDefinition] -> [(String,Int)]
findFunctions [] = []
findFunctions ((name,_,_):xs) = (name,0) : findFunctions xs

frst :: (a, b, c) -> a
frst (a,_,_) = a

scnd :: (a, b, c) -> b
scnd (_,b,_) = b

thrd :: (a, b, c) -> c
thrd (_,_,c) = c