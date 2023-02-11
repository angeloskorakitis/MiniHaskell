{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Transform (transform) where

import Types
import GHC.Plugins (all2)
import GHC.Tc.Utils.Monad (whenNoErrs)

-- TODO: Implement this function

-----------------------------------------------------------------------------------------
------------------------------- Convert FExpr to IExpr ----------------------------------
-----------------------------------------------------------------------------------------

-- Ousiastika mazi me to FExpr

transformExpr :: FExpr -> [(String,Int)] -> IExpr
transformExpr (FVar s) _ = IVar s 
transformExpr (FNum n) _ = INum n
transformExpr (FBool b) _ = IBool b
transformExpr (FParens b) occurs = IParens (transformExpr b occurs)
transformExpr (FIfThenElse i1 i2 i3) occurs = IIfThenElse (transformExpr i1 occurs) (transformExpr i2 occurs) (transformExpr i3 occurs)
-- Μαλλον στο ICall βαζουμε την μετρηση του ποιο ειναι το occurence της συναρτησης
-- Να δω επισης IActuals
transformExpr (FCall f p) occurs = ICall (occurence f occurs) f
        where
        occurence :: String -> [(String,Int)] -> Int
        occurence f [] = 0
        occurence f ((s,n):xs) = if f == s then n else occurence f xs



transformExpr (FCompOp op i1 i2)  occurs = ICompOp op (transformExpr i1 occurs) (transformExpr i2 occurs)
transformExpr (FBinaryOp op i1 i2)  occurs = IBinaryOp op (transformExpr i1 occurs) (transformExpr i2 occurs)
transformExpr (FBooleanOp op i1 i2) occurs = IBooleanOp op (transformExpr i1 occurs) (transformExpr i2 occurs)
transformExpr (FUnaryOp op i) occurs = IUnaryOp op (transformExpr i occurs)

transformDef :: FDefinition -> IDefinition
transformDef (name, _, expr) = (name, transformExpr expr [])

-----------------------------------------------------------------------------------------
----------------------- The construction of the programTransform ------------------------
-----------------------------------------------------------------------------------------
-- findsss :: FProgram -> [(String,Int)]
-- findsss (expr, defs) = findFs defs

-- findFs :: [FDefinition] -> [(String,Int)]
-- findFs [] = []
-- findFs ((name, _, _):xs) = (name, 0) : (findFs xs)


countFCalls :: FProgram -> [(String,Int)]
countFCalls (expr, defs) =  a1
        where 
        a1 = countFCallsExpr expr []
        a2 = countFCallsDefs defs a1


countFCallsExpr :: FExpr -> [(String,Int)] ->[(String,Int)]
countFCallsExpr (FVar s) _ = []
countFCallsExpr (FNum n) _ = []
countFCallsExpr (FBool b) _ = []
countFCallsExpr (FParens b) lookup = countFCallsExpr b lookup
countFCallsExpr (FIfThenElse i1 i2 i3) lookup = a ++ (countFCallsExpr i2 a) ++ (countFCallsExpr i3 a)
        where a = (countFCallsExpr i1 (countFCallsExpr i2 (countFCallsExpr i3 lookup)))

countFCallsExpr (FCall f p) lookup = ([(f, (occurence f nlookup))]) ++ (countFCallsExprs p nlookup)
        where nlookup = (increase_occurence f lookup)

countFCallsExpr (FCompOp op i1 i2) lookup = a ++ (countFCallsExpr i2 a)
        where a = (countFCallsExpr i1 (countFCallsExpr i2 lookup))
countFCallsExpr (FBinaryOp op i1 i2) lookup = a ++ (countFCallsExpr i2 a)
        where a = (countFCallsExpr i1 (countFCallsExpr i2 lookup))
countFCallsExpr (FBooleanOp op i1 i2) lookup = a ++ (countFCallsExpr i2 a)
        where a = (countFCallsExpr i1 (countFCallsExpr i2 lookup))
countFCallsExpr (FUnaryOp op i) lookup = countFCallsExpr i lookup

-- countFCallsExprs :: [FExpr] -> [(String,Int)] ->[(String,Int)]
-- countFCallsExprs [] _ = []
-- countFCallsExprs (x:xs) lookup = (countFCallsExpr x a) ++ (countFCallsExprs xs a)
--         where a = (countFCallsExprs xs lookup)

-- countFCallsDefs :: [FDefinition] -> [(String,Int)] -> [(String,Int)]
-- countFCallsDefs [] _ = []
-- countFCallsDefs ((f,_,expr):xs) lookup = (countFCallsExpr expr lookup) ++ (countFCallsDefs xs a)
--         where a = (countFCallsDefs xs lookup)

countFCallsExprs :: [FExpr] -> [(String, Int)] -> [(String, Int)]
countFCallsExprs [] lookup = []
countFCallsExprs (x:xs) lookup = countFCallsExpr x lookup ++ countFCallsExprs xs lookup

countFCallsDefs :: [FDefinition] -> [(String, Int)] -> [(String, Int)]
countFCallsDefs [] lookup = []
countFCallsDefs ((f, _, expr):xs) lookup = countFCallsExpr expr newLookup ++ countFCallsDefs xs newLookup
  where newLookup = (f, 0) : lookup ++ countFCallsDefs xs lookup

increase_occurence :: String -> [(String,Int)] -> [(String,Int)]
increase_occurence f [] = [(f,0)]
increase_occurence f ((s,n):xs) = if f == s then (s,n+1):xs else (s,n):(increase_occurence f xs)

occurence :: String -> [(String,Int)] -> Int
occurence f [] = 0
occurence f ((s,n):xs) = if f == s then n else occurence f xs


transform :: FProgram -> IProgram
transform (expr, defs) = iexpr : idefs
    where   iexpr = ("result", transformExpr expr [])
            idefs = map transformDef defs
-- transform = error "'transform' not implemented!"