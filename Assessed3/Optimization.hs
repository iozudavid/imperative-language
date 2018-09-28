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
                | otherwise = m j

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
optExpr f (Constant n)    = Constant n
optExpr f (Var i)         = case f i of Nothing -> Var i
                                        Just x  -> Constant x
optExpr f (Op Add xs)     = if(optExpr f (head xs)==Constant 0) then optExpr f (head(tail xs))
                            else if(optExpr f (head (tail xs))==Constant 0) then optExpr f (head xs)
                            else if(isChangeable f (head xs) && isChangeable f (head(tail xs))) then Constant (fromConstant(optExpr f (head xs)) + fromConstant(optExpr f (head(tail xs))))
                            else Op Add [optExpr f (head xs), optExpr f (head(tail xs))]
optExpr f (Op Sub xs)     = if(isChangeable f (head xs) && isChangeable f (head(tail xs))) then Constant (fromConstant(optExpr f (head xs)) - fromConstant(optExpr f (head(tail xs))))
                            else Op Sub [optExpr f (head xs), optExpr f (head(tail xs))]
optExpr f (Op Mul xs)     = if(optExpr f (head xs)==Constant 0 || optExpr f (head(tail xs))==Constant 0) then Constant 0
                            else if(optExpr f (head xs)==Constant 1) then optExpr f (head(tail xs))
                            else if(optExpr f (head(tail xs))==Constant 1) then optExpr f (head xs)
                            else if(isChangeable f (head xs) && isChangeable f (head(tail xs))) then Constant (fromConstant(optExpr f (head xs)) * fromConstant(optExpr f (head(tail xs))))
                            else Op Mul [optExpr f (head xs), optExpr f (head(tail xs))]
optExpr f (Op Div xs)     = if(isChangeable f (head xs) && isChangeable f (head(tail xs))) then Constant (fromConstant(optExpr f (head xs)) `div` fromConstant(optExpr f (head(tail xs))))
                            else Op Div [optExpr f (head xs), optExpr f (head(tail xs))]
optExpr f (Op Mod xs)     = if(isChangeable f (head xs) && isChangeable f (head(tail xs))) then Constant (fromConstant(optExpr f (head xs)) `mod` fromConstant(optExpr f (head(tail xs))))
                            else Op Mod [optExpr f (head xs), optExpr f (head(tail xs))]                         
optExpr f (Op And xs)     = if(optExpr f (head xs)==Constant 0 || optExpr f (head(tail xs))==Constant 0) then Constant 0
                            else if(optExpr f (head xs)==Constant 1) then optExpr f (head(tail xs))
                            else if(optExpr f (head(tail xs))==Constant 1) then optExpr f (head xs)
                            else if(isChangeable f (head xs) && isChangeable f (head(tail xs))) then Constant (number(boolean(fromConstant(optExpr f (head xs))) && boolean(fromConstant(optExpr f (head(tail xs))))))
                            else Op And [optExpr f (head xs), optExpr f (head(tail xs))]
optExpr f (Op Or xs)      = if(optExpr f (head xs)==Constant 1 || optExpr f (head(tail xs))==Constant 1) then Constant 1
                            else if(isChangeable f (head xs) && isChangeable f (head(tail xs))) then Constant (number(boolean(fromConstant(optExpr f (head xs))) || boolean(fromConstant(optExpr f (head(tail xs))))))
                            else Op Or [optExpr f (head xs), optExpr f (head(tail xs))]
optExpr f (Op Not xs)     = if(optExpr f (head xs)==Constant 0) then Constant 1
                            else if(isNotC0(optExpr f (head xs))) then Constant 0
                            else Op Not [optExpr f (head xs)]
optExpr f (Op Eq xs)      = if(isChangeable f (head xs) && isChangeable f (head(tail xs))) then Constant (number(fromConstant(optExpr f (head xs)) == fromConstant(optExpr f (head(tail xs)))))
                            else Op Eq [optExpr f (head xs), optExpr f (head(tail xs))]
optExpr f (Op Leq xs)     = if(isChangeable f (head xs) && isChangeable f (head(tail xs))) then Constant (number(fromConstant(optExpr f (head xs)) <= fromConstant(optExpr f (head(tail xs)))))
                            else Op Leq [optExpr f (head xs), optExpr f (head(tail xs))]
optExpr f (Op Less xs)    = if(isChangeable f (head xs) && isChangeable f (head(tail xs))) then Constant (number(fromConstant(optExpr f (head xs)) < fromConstant(optExpr f (head(tail xs)))))
                            else Op Less [optExpr f (head xs), optExpr f (head(tail xs))]
optExpr f (Op Geq xs)     = if(isChangeable f (head xs) && isChangeable f (head(tail xs))) then Constant (number(fromConstant(optExpr f (head xs)) >= fromConstant(optExpr f (head(tail xs)))))
                            else Op Geq [optExpr f (head xs), optExpr f (head(tail xs))]
optExpr f (Op Greater xs) = if(isChangeable f (head xs) && isChangeable f (head(tail xs))) then Constant (number(fromConstant(optExpr f (head xs)) > fromConstant(optExpr f (head(tail xs)))))
                            else Op Greater [optExpr f (head xs), optExpr f (head(tail xs))]

isNotC0 :: Expr -> Bool
isNotC0 (Constant 0) = False
isNotC0 (Constant _) = True
isNotC0 _            = False



optProgram  :: Program -> OptStorage -> (Program, OptStorage)
optProgram p f = case optProgram'' p f of (Just x, f')  -> (eliminateBlocksInBlocks x, f')
                                          (Nothing, f') -> (Block [], f')

-- This is what we are really interested in in practice:  
  
optProgram' :: Program -> Program
optProgram' p = fst(optProgram p emptyOptStorage)

isChangeable :: OptStorage -> Expr -> Bool
isChangeable f (Constant n) = True
isChangeable f (Var i)      = case f i of Nothing -> False
                                          Just x  -> True
isChangeable f (Op g xs)    = allBool [isChangeable f x | x <- xs]

allBool :: [Bool] -> Bool
allBool [] = True
allBool (x:xs) = x && (allBool xs)


optProgram'' :: Program -> OptStorage -> (Maybe Program, OptStorage)
optProgram'' (i := e) f         = (Just (i := (optExpr f e)), (optUpdate i (optExpr f e) f))
optProgram'' (Block xs) f       = {--if (xs == []) then (Nothing, f) else if(fst(applyToAll xs [] f) == []) then (Nothing, f) else--} (Just(Block (fst(applyToAll xs [] f))),snd(applyToAll xs [] f))
optProgram'' (While e p) f      = if(optExpr f e == Constant 0) then (Nothing, f) else if(updatedVariables p == []) then case optProgram'' p f of (Just x, f') -> (Just(While (optExpr f e) x), f)
                                                                                                                                                  (Nothing, f') -> (Nothing, f)
                                                                                  else case p of (Block w) -> let interm = (deleteFromBlock w (decideToDelete (forWhileUse w f []) []) []) in 
                                                                                                              let newStorage = (deleteVars (updatedVariables (While e (Block interm))) f) in
                                                                                                             (Just (While (optExpr newStorage e) (Block(fst(applyToAll' interm [] newStorage)))), newStorage)
                                                                                  --(Just (While e (Block (deleteFromBlock w (decideToDelete (forWhileUse w f []) []) [])) ), f) --of (Just r, f')  -> (Just(While e r), f')
                                                                                                                                                                                                     --       (Nothing, f') -> (Nothing, f') 
                                                                                                 _         -> let newStorage = (deleteVars (updatedVariables (While e p)) f) in
                                                                                                              case optProgram'' p f of (Just x, f') -> (Just(While (optExpr newStorage e) x), newStorage)
                                                                                                                                       (Nothing, f') -> (Nothing, f)
optProgram'' (If e p) f         = if(optExpr f e == Constant 0) then (Nothing, f) else case optExpr f e of Constant _ -> optProgram'' p f 
                                                                                                           _          -> case (optProgram'' p f) of (Just x, f')  -> let newStorage = (deleteVars (updatedVariables (If e p)) f) in 
                                                                                                                                                                     (Just (If (optExpr f e) x), newStorage)
                                                                                                                                                    (Nothing, f') -> (Nothing, f)
optProgram'' (IfElse e p1 p2) f = if(optExpr f e == Constant 0) then (optProgram'' p2 f) else case optExpr f e of Constant _ -> optProgram'' p1 f
                                                                                                                  _          -> case (optProgram'' p1 f) of (Just x, f')  -> case (optProgram'' p2 f) of (Just x', f'') -> let newStorage = (deleteVars (updatedVariables (IfElse e p1 p2)) f) in
                                                                                                                                                                                                                           ((Just (IfElse (optExpr f e) x x')),newStorage)
                                                                                                                                                                                                         (Nothing, f'') -> let newStorage = (deleteVars (updatedVariables (If e p1)) f) in
                                                                                                                                                                                                                           ((Just (IfElse (optExpr f e) x (Block []))), newStorage)
                                                                                                                                                            (Nothing, f') -> case (optProgram'' p2 f) of (Just x', f'') -> let newStorage = (deleteVars (updatedVariables (If e p2)) f) in
                                                                                                                                                                                                                           ((Just (IfElse (optExpr f e) (Block []) x'), newStorage))
                                                                                                                                                                                                         (Nothing, f'') -> ((Just (IfElse (optExpr f e) (Block []) (Block []))), f)
optProgram'' (Read i) f         = (Just(Read i), deleteVar i f)
optProgram'' (Write e) f        = (Just(Write e), f)
optProgram'' (Print s) f        = (Just(Print s), f)

applyToAll :: [Program] -> [Program] -> OptStorage -> ([Program], OptStorage)
applyToAll [] g f = (g, f)
applyToAll (x:xs) g f = case (optProgram'' x f) of (Nothing, q) -> applyToAll xs g f
                                                   (Just z, y)  -> applyToAll xs (g++[z]) y

applyToAll' :: [Program] -> [Program] -> OptStorage -> ([Program], OptStorage)
applyToAll' [] g f = (g, f)
applyToAll' (x:xs) g f = case (optProgram'' x f) of (Nothing, q) -> applyToAll xs (g++[x]) q
                                                    (Just z, y)  -> applyToAll xs (g++[z]) y

forWhileUse :: [Program] -> OptStorage -> [Program] -> ([Program], OptStorage)
forWhileUse [] f xs     = (xs, f)
forWhileUse (x:xs) f ys = case (optProgram'' x f) of (Just w, f')  -> forWhileUse xs f' ys
                                                     (Nothing, f') -> forWhileUse xs f' (ys++[x]) 

decideToDelete :: ([Program], OptStorage) -> [Program] -> [Program]
decideToDelete ([], f) xs     = xs 
decideToDelete ((x:xs), f) ys = case (optProgram'' x f) of (Just w, f')  -> decideToDelete (xs, f) ys 
                                                           (Nothing, f') -> decideToDelete (xs, f) (ys++[x])

deleteFromBlock :: [Program] -> [Program] -> [Program] -> [Program]
deleteFromBlock [] ys zs     = zs 
deleteFromBlock (x:xs) ys zs = if(contains x ys) then deleteFromBlock xs ys zs
                               else deleteFromBlock xs ys (zs++[x])

contains :: Program -> [Program] -> Bool
contains p []     = False
contains p (x:xs) = (p == x) || (contains p xs)

eliminateBlocksInBlocks :: Program -> Program
eliminateBlocksInBlocks (Block xs)       = Block (eliminateBlocksInBlocks' (xs) [])
eliminateBlocksInBlocks (While e p)      = While e (eliminateBlocksInBlocks p)
eliminateBlocksInBlocks (If e p)         = If e (eliminateBlocksInBlocks p)
eliminateBlocksInBlocks (IfElse e p1 p2) = IfElse e (eliminateBlocksInBlocks p1) (eliminateBlocksInBlocks p2)
eliminateBlocksInBlocks x                = x

eliminateBlocksInBlocks' :: [Program] -> [Program] -> [Program]
eliminateBlocksInBlocks' [] xs     = xs
eliminateBlocksInBlocks' (x:xs) ys = case x of (Block zs)       -> eliminateBlocksInBlocks' xs (ys ++ zs)
                                               (While e p)      -> eliminateBlocksInBlocks' xs (ys ++ [While e (eliminateBlocksInBlocks p)])
                                               (If e p)         -> eliminateBlocksInBlocks' xs (ys ++ [If e (eliminateBlocksInBlocks p)])
                                               (IfElse e p1 p2) -> eliminateBlocksInBlocks' xs (ys ++ [IfElse e (eliminateBlocksInBlocks p1) (eliminateBlocksInBlocks p2)])
                                               y                -> eliminateBlocksInBlocks' xs (ys ++ [y])

