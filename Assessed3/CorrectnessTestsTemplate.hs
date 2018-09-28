{-# LANGUAGE Safe #-} 

{-
   Some rudimentary sample tests.

   Warning: passing these tests is no guarantee of correctness.
   Failing any of them should be, however, a guarantee of
   incorrectness.

   The marking program may find errors that are not accounted for
   here. The tests here are only to give you ideas for testing. There
   is no guarantee that if your submission passes all tests here it
   will necessarily pass our marking tests.

   This file need not be submitted, and if it is submitted it will be
   ignored.

    Feel free to modify it for your own testing purposes, after you
    rename the file to CorrectnessTests.

-}

module CorrectnessTests where

import AbstractSyntax
import Parsing
import Parser
import Interpreter
import PrettyPrinting
import IOPrime
import Optimization


-- Parsing:

parseReadTest1 :: Bool
parseReadTest1 = parse program "read x;"
                 == [(Read "x","")]

parseReadTest2 :: Bool
parseReadTest2 = parse program "read x;   rest of the program"
                 == [(Read "x","rest of the program")]

parseWriteTest1 :: Bool
parseWriteTest1 = parse program "write (x+y*z);"
                  == [(Write (Op Add [Var "x",Op Mul [Var "y",Var "z"]]),"")]

parseWriteTest2 :: Bool
parseWriteTest2 = parse program "write (x+y*z);   rest of the program"
                  == [(Write (Op Add [Var "x",Op Mul [Var "y",Var "z"]]),"rest of the program")]

parsePrintTest1 :: Bool
parsePrintTest1 = parse program "print \"hello world\";"
                  == [(Print "hello world","")]

parsePrintTest2 :: Bool
parsePrintTest2 = parse program "print \"hello world\";   rest of the program"
                  == [(Print "hello world","rest of the program")]

parseTest :: Bool
parseTest = parseReadTest1 &&
            parseReadTest2 &&
            parseWriteTest1 &&
            parseWriteTest2 &&
            parsePrintTest1 &&
            parsePrintTest2

-- Running.  The whole point of using IO' rather than IO is that now
-- we can programs with values on IO'. We use pipeR for that purpose.

assignmentTest1 :: Bool
assignmentTest1 = s == "" && m "x" == 3+7*6
 where
   p = "x" := Op Add [Constant 3, Op Mul[Constant 7,Constant 6]]
   (s, m) = pipeR (run p emptyStorage) ""

assignmentTest2 :: Bool
assignmentTest2 = s == "" && m "x" == 3+7*6 && m "zero" == 0
 where
   p = "x" := Op Add [Constant 3, Op Mul[Constant 7,Constant 6]]
   (s, m) = pipeR (run p (const 0)) ""

blockTest1 :: Bool
blockTest1 = s == "" && m "x" == 17
 where
   (s, m) = pipeR (run (Block []) (update "x" 17 emptyStorage)) ""

blockTest2 :: Bool
blockTest2 = s == "" && m "x" == 17 && m "apples" == 6
 where
   (s, m) = pipeR (run (Block []) (update "x" 17 (fromIntegral.length))) ""

blockTest3 :: Bool
blockTest3 = s == "" && m "x" == 17*2
 where
   p = "x" := Constant 17
   q = "x" := Op Mul [Var "x" , Constant 2]
   (s, m) = pipeR (run (Block [p,q]) emptyStorage) ""

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

writeTest :: Bool
writeTest = s == "17\n" && m "x" == 9
 where
   p = Write (Constant 17)
   (s, m) = pipeR (run p (const 9)) ""

readTest1 :: Bool
readTest1 = s == "" && m "x" == 17
 where
   p = Read "x"
   (s, m) = pipeR (run p emptyStorage) "17\n"

readTest2 :: Bool
readTest2 = s == "" && m "x" == 17 && m "y" == 9
 where
   p = Read "x"
   (s, m) = pipeR (run p (const 9)) "17\n"

printTest :: Bool
printTest = s == "17\n" && m "five." == 5
 where
   p = Print "17"
   (s, m) = pipeR (run p (fromIntegral.length)) "17\n"

statementsTest :: Bool
statementsTest = assignmentTest1 &&
                 assignmentTest2 &&
                 blockTest1 &&
                 blockTest2 &&
                 blockTest3 &&
                 ifTest1 &&
                 ifTest2 &&
                 ifTest3 &&
                 ifTest4 &&
                 ifElseTest1 &&
                 ifElseTest2 &&
                 ifElseTest3 &&
                 ifElseTest4 &&
                 whileTest1 &&
                 whileTest2 &&
                 writeTest &&
                 readTest1 &&
                 readTest2 &&
                 printTest
  
-- Testing realistic programs.

fibonacci :: String
fibonacci = "{\n\
 \print \"please input a number\";\n\
 \read x;\n\
 \y := 1;\n\
 \z := 1;\n\
 \while (x > 0)\n\
 \ {\n\
 \  x := x - 1;\n\
 \  t := y + z;\n\
 \  y := z;\n\
 \  z := t;\n\
 \ }\n\
 \print \"its fibonacci value is\";\n\
 \write y;\n\ 
 \}"

fibonacciProgram :: Program
fibonacciProgram =
  Block [Print "please input a number",
         Read "x",
         "y" := Constant 1,
         "z" := Constant 1,
         While (Op Greater [Var "x",Constant 0])
           (Block ["x" := Op Sub [Var "x",Constant 1],
           "t" := Op Add [Var "y",Var "z"],
           "y" := Var "z",
           "z" := Var "t"]),
         Print "its fibonacci value is",
         Write (Var "y")]


fibonacciParseTest :: Bool
fibonacciParseTest = parse program fibonacci == [(fibonacciProgram,"")]

fibonacciIO' :: IO' ()
fibonacciIO' = runIO' fibonacciProgram

fibonacciPipe :: String -> String
fibonacciPipe = pipe fibonacciIO'

fibonacciIO :: IO () -- for manual testing
fibonacciIO = interact fibonacciPipe

fibonacciRunTest :: Bool
fibonacciRunTest = length l == 3
                && l !! 0 == "please input a number"
                && l !! 1 == "its fibonacci value is"
                && l !! 2 == "89"
 where
   l = lines(fibonacciPipe "10\n")
   
-- second file test

factorial :: String
factorial = "{\n\
  \print \"please input a number\";\n\
  \read x;\n\
  \y := 1;\n\
  \while (x > 0)\n\
  \ {\n\
  \ y := y * x;\n\
  \ x := x - 1;\n\
  \ }\n\
  \print \"its factorial is\";\n\
  \write y;\n\
  \}\n"

factorialProgram :: Program
factorialProgram =
  Block [Print "please input a number",
         Read "x",
         "y" := Constant 1,
         While (Op Greater [Var "x",Constant 0])
           (Block ["y" := Op Mul [Var "y",Var "x"],
                   "x" := Op Sub [Var "x",Constant 1]]),
         Print "its factorial is",
         Write (Var "y")]

factorialParseTest :: Bool
factorialParseTest = parse program factorial == [(factorialProgram,"")]

factorialIO' :: IO' ()
factorialIO' = runIO' factorialProgram

factorialPipe :: String -> String
factorialPipe = pipe factorialIO'

factorialIO :: IO () -- for manual testing
factorialIO = interact factorialPipe

factorialRunTest :: Bool
factorialRunTest = length l == 3
                && l !! 0 == "please input a number"
                && l !! 1 == "its factorial is"
                && l !! 2 == "3628800"
 where
   l = lines(factorialPipe "10\n")

-- third file test

isprime :: String
isprime = "{\n\
  \print \"please input a number\";\n\
  \read x;\n\
  \\n\
  \p := x >= 2;\n\
  \n := 2;\n\
  \\n\
  \while (n * n <= x && p) {\n\
  \  p := x % n;\n\
  \  n := n + 1;\n\
  \}\n\
  \\n\
  \if (p)\n\
  \  print \"the number is prime\";\n\
  \else {\n\
  \  print \"the number is divisible by\";\n\
  \  write (n-1);\n\
  \}\n\
  \}\n"


isprimeProgram :: Program
isprimeProgram =
  Block [Print "please input a number",
         Read "x",
         "p" := Op Geq [Var "x",Constant 2],
         "n" := Constant 2,
         While (Op And [Op Leq [Op Mul [Var "n",Var "n"],Var "x"],Var "p"])
           (Block ["p" := Op Mod [Var "x",Var "n"],
                   "n" := Op Add [Var "n",Constant 1]]),
         IfElse (Var "p")
           (Print "the number is prime")
           (Block [Print "the number is divisible by",
                   Write (Op Sub [Var "n",Constant 1])])]

isprimeParseTest :: Bool
isprimeParseTest = parse program isprime == [(isprimeProgram,"")]

isprimeIO' :: IO' ()
isprimeIO' = runIO' isprimeProgram

isprimePipe :: String -> String
isprimePipe = pipe isprimeIO'

isprimeIO :: IO () -- for manual testing
isprimeIO = interact isprimePipe

isprimeRunTest1 :: Bool
isprimeRunTest1 = length l == 2
               && l !! 0 == "please input a number"
               && l !! 1 == "the number is prime"
 where
   l = lines(isprimePipe "17\n")

isprimeRunTest2 :: Bool
isprimeRunTest2 = length l == 3
                && l !! 0 == "please input a number"
                && l !! 1 == "the number is divisible by"
                && l !! 2 == "2"
 where
   l = lines(isprimePipe "170\n")

allFilesTest :: Bool
allFilesTest = fibonacciParseTest && fibonacciRunTest &&
               factorialParseTest && factorialRunTest &&
               isprimeParseTest && isprimeRunTest1 && isprimeRunTest2

allTests :: Bool
allTests = parseTest && statementsTest && allFilesTest

-- This doesn't test the keywords question.

debugFib :: String
debugFib =
 "{\n\
 \debug := 0;\n\
 \print \"please input a number\";\n\
 \read x;\n\
 \y := 1;\n\
 \z := 1;\n\
 \while (x > 0)\n\
 \ {\n\
 \  if (debug) {\n\
 \     write x;\n\
 \     write y;\n\
 \     write t;\n\
 \  }\n\
 \  x := x - 1;\n\
 \  t := y + z;\n\
 \  y := z;\n\
 \  z := t;\n\
 \ }\n\
 \print \"its fibonacci value is\";\n\
 \write y;\n\ 
 \}"

testOptFib = fst(optProgram(parseProgram debugFib) emptyOptStorage)

debugSmall :: String
debugSmall =
 "{\n\
 \debug := 0;\n\
 \  if (debug) {\n\
 \     write x;\n\
 \     write y;\n\
 \     write t;\n\
 \  }\n\
 \}"

testOptSmall = fst(optProgram(parseProgram debugSmall) emptyOptStorage)

debugSmallSmall :: String
debugSmallSmall = "if (0) x:=1;"

testOptSmallSmall = fst(optProgram(parseProgram debugSmallSmall) emptyOptStorage)
