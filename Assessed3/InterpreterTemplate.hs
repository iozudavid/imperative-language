{-# LANGUAGE Safe #-} 

{-
   PLESE READ CAREFULLY:

   You need to replace the occurrences of "question" by your own code
   to solve the questions. If you don't attempt a question, leave it
   as a question.

   For this file to qualify for marking:

     * The option "Safe" should not be removed.
     * The given code should not be modified.
     * It must compile without errors.

   Submissions departing from these requirement will not be considered.

   Search for "question" to find what you need to do.
-}

module Interpreter where

import Unsolved
import AbstractSyntax
import IOPrime

type Storage = Identifier -> Integer

emptyStorage :: Storage
emptyStorage i = error ("Uninitialized identifier " ++ i)

update :: Identifier -> Integer -> Storage -> Storage
update i x m = m'
 where
   m' :: Storage
   m' j | i == j    = x
        | otherwise = m j

number :: Bool -> Integer
number False = 0
number True  = 1

boolean :: Integer -> Bool
boolean 0 = False
boolean _ = True

opEval :: OpName -> [Integer] -> Integer
opEval Add     [x, y] = x + y
opEval Sub     [x, y] = x - y
opEval Mul     [x, y] = x * y
opEval Div     [x, y] = x `div` y
opEval Mod     [x, y] = x `mod` y
opEval Eq      [x, y] = number(x == y)
opEval Leq     [x, y] = number(x <= y)
opEval Less    [x, y] = number(x <  y)
opEval Geq     [x, y] = number(x >= y)
opEval Greater [x, y] = number(x >  y)
opEval And     [x, y] = number(boolean x && boolean y)
opEval Or      [x, y] = number(boolean x || boolean y)
opEval Not     [x]    = number(not(boolean x))
opEval op      xs     = error ("Interpreter bug. "
                            ++ "Please contact the software maintainer. "
                            ++ "Tried to apply " ++ show op
                            ++ " to " ++ show xs)

eval :: Storage -> Expr ->  Integer
eval m (Constant x) = x
eval m (Var i)      = m i
eval m (Op o es)    = opEval o [eval m e | e <- es]

-- Questions 1-9. Implement the function run. You may leave undefined
-- the cases that you don't know how to define. You may get partial
-- marks if only some of them are correctly defined.

run :: Program -> Storage -> IO' Storage

run (i := e)          m = return (update i (eval m e) m)

run (IfElse e p q)    m = if(boolean(eval m e)) then run p m else run q m 

run (If e p)          m = if(boolean(eval m e)) then run p m else return m

run (While e p)       m = if(boolean(eval m e)) then do
                                                       m' <- run p m
                                                       run (While e p) (m') 
                                                else return m 

run (Block [])        m = return m

run (Block (p : ps))  m = do
                             m' <- run p m
                             run (Block ps) (m') 

run (Read i)          m = 
  do
     input <- getLine'
     let n = read input :: Integer
     run (i := (Constant n)) m

run (Write e)         m = case e of (Constant n) -> do
                                                       let s = show n
                                                       putStrLn' s
                                                       return m
                                    (Var i) -> do
                                                  putStrLn' i
                                                  return m
                                    (Op n l) -> case n of Not -> do
                                                                   let s = show (head l)
                                                                   putStrLn' ((p Not) ++ s)
                                                                   return m
                                                          s   -> do
                                                                   let s1 = show (head l)
                                                                   let s2 = show (head(tail l))
                                                                   putStrLn' (s1++(p s)++s2)
                                                                   return m

run (Print s)         m = do
                             putStrLn' s
                             return m


p :: OpName -> String
p Or = "||"
p And = "&&"
p Eq = "=="
p Leq = "<="
p Less = "<" 
p Geq = ">="
p Greater = ">"
p Add = "+"
p Sub = "-"
p Mul = "*"
p Div = "/"
p Mod = "%"                    
p Not = "!"                                

-- We use this for testing in the file CorrectnessTests.hs:
runIO' :: Program -> IO'()
runIO' p = fmap (\m -> ()) (run p emptyStorage)

ifTest1 :: Bool
ifTest1 = s == "" && m "x" == 17
 where
   p = If (Constant 1) ("x" := Constant 17)
   (s, m) = pipeR (run p emptyStorage) ""

ifTest2 :: Bool
ifTest2 = s == "" && m "x" == 13
 where
   p = If (Constant 0) ("x" := Constant 17)
   (s, m) = pipeR (run p (update "x" 13 emptyStorage)) ""

ifTest3 :: Bool
ifTest3 = s == "" && m "x" == 17 && m "birmingham" == 98
 where
   p = If (Constant 1) ("x" := Constant 17)
   (s, m) = pipeR (run p (fromIntegral . fromEnum . head)) ""

ifTest4 :: Bool
ifTest4 = s == "" && m "x" == 13 && m "birmingham" == 98
 where
   p = If (Constant 0) ("x" := Constant 17)
   (s, m) = pipeR (run p (update "x" 13 (fromIntegral . fromEnum . head))) ""

ifElseTest1 :: Bool
ifElseTest1 = s == "" && m "x" == 17
 where
   p = IfElse (Constant 1) ("x" := Constant 17) ("x" := Constant 13)
   (s, m) = pipeR (run p emptyStorage) ""

ifElseTest2 :: Bool
ifElseTest2 = s == "" && m "x" == 13
 where
   p = IfElse (Constant 0) ("x" := Constant 17) ("x" := Constant 13)
   (s, m) = pipeR (run p emptyStorage) ""

ifElseTest3 :: Bool
ifElseTest3 = s == "" && m "x" == 17 && m "abc" == 3
 where
   p = IfElse (Constant 1) ("x" := Constant 17) ("x" := Constant 13)
   (s, m) = pipeR (run p (fromIntegral.length)) ""

ifElseTest4 :: Bool
ifElseTest4 = s == "" && m "x" == 13 && m "abc" == 3
 where
   p = IfElse (Constant 0) ("x" := Constant 17) ("x" := Constant 13)
   (s, m) = pipeR (run p (fromIntegral.length)) ""

whileTest1 :: Bool
whileTest1 = s == "" && m "x" == 0
 where
   p = While (Var "x") ("x" := Op Sub [Var "x",Constant 1])
   (s, m) = pipeR (run p (update "x" 17 emptyStorage)) ""

whileTest2 :: Bool
whileTest2 = s == "" && m "x" == 0 && m "four" == 4
 where
   p = While (Var "x") ("x" := Op Sub [Var "x",Constant 1])
   (s, m) = pipeR (run p (update "x" 17 (fromIntegral.length))) ""









