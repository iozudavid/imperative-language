module Optimization where

import Data.Maybe
import Data.List

import Unsolved
import AbstractSyntax
import Interpreter

type OptStorage = Identifier -> Maybe Integer

emptyOptStorage :: OptStorage
emptyOptStorage i = Nothing

optUpdate :: Identifier -> Expr -> OptStorage -> OptStorage
optUpdate i e m = case e of
                  Constant x -> m' x
                  _          -> m''
 where
   m' x j | i == j    = Just x
          | otherwise = m j
   m'' j  | i == j    = Nothing
          | otherwise = m j

deleteVar :: Identifier -> OptStorage -> OptStorage
deleteVar i m j | i == j    = Nothing
                | otherwise = m i

deleteVars :: [Identifier] -> OptStorage -> OptStorage
deleteVars [] m = m
deleteVars (i:is) m = deleteVars is (deleteVar i m)


fromConstant :: Expr -> Integer
fromConstant (Constant x) = x
fromConstant _            = error "program bug"

isConstant :: Expr -> Bool
isConstant (Constant x) = True
isConstant _            = False

updatedVariables, updatedVariables'  :: Program -> [Identifier]
updatedVariables                 = nub . updatedVariables'
updatedVariables' (i := e)       = [i]
updatedVariables' (IfElse e p q) = updatedVariables' p ++ updatedVariables' q
updatedVariables' (If e p)       = updatedVariables' p
updatedVariables' (While e p)    = updatedVariables' p
updatedVariables' (Block ps)     = concat(map updatedVariables' ps)
updatedVariables' (Read i)       = [i]
updatedVariables' (Write e)      = []
updatedVariables' (Print s)      = []


-- You don't need to use the above functions. But you may find them
-- useful. Indeed, we use them in our sample solution.

optExpr :: OptStorage -> Expr -> Expr
optExpr f (Constant n) = Constant n
optExpr f (Var i)      = case f i of Nothing -> Var i
                                     Just x  -> Constant x
optExpr f (Op Add xs)  = if(isChangeable) then Constant (fromConstant(optExpr f (head xs)) + fromConstant(optExpr f (head(tail xs))))
                         else Op Add [optExpr f (head xs), optExpr f (head(tail xs))]   

optProgram  :: Program -> OptStorage -> (Program, OptStorage)

optProgram = question "optimize program"

-- This is what we are really interested in in practice:  
  
optProgram' :: Program -> Program
optProgram' p = fst(optProgram p emptyOptStorage)

isChangeable :: OptStorage -> Expr -> Bool
isChangeable f (Constant n) = True
isChangeable f (Var i)      = case f i of Nothing -> False
                                          Just x  -> True
isChangeable f (Op f xs)    = allBool [isChangeable f x | x <- xs]

allBool :: [Bool] -> Bool
allBool [] = True
allBool (x:xs) = x && (allBool xs)




