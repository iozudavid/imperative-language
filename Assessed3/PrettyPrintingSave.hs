module PrettyPrinting where


import Unsolved
import AbstractSyntax

spaces n = take n (repeat ' ')

ppExpr :: Expr -> String
ppExpr (Constant n) = show n
ppExpr (Var i) = i
ppExpr (Op n []) = undefined
ppExpr (Op n (xs)) = case n of Or       -> (decide or' (head xs))         ++ p Or      ++ (decide or' (head(tail xs)))
                               And      -> (decide and' (head xs))        ++ p And     ++ (decide and' (head(tail xs)))
                               Eq       -> (decide eq' (head xs))         ++ p Eq      ++ (decide eq' (head(tail xs)))
                               Leq      -> (decide eq' (head xs))         ++ p Leq     ++ (decide eq' (head(tail xs)))
                               Less     -> (decide eq' (head xs))         ++ p Less    ++ (decide eq' (head(tail xs)))
                               Geq      -> (decide eq' (head xs))         ++ p Geq     ++ (decide eq' (head(tail xs)))
                               Greater  -> (decide eq' (head xs))         ++ p Greater ++ (decide eq' (head(tail xs)))
                               Add      -> (decide primaryop (head xs))   ++ p Add     ++ (decide primaryop (head(tail xs)))
                               Sub      -> (decide primaryop (head xs))   ++ p Sub     ++ (decide primaryop (head(tail xs)))
                               Mul      -> (decide secondaryop (head xs)) ++ p Mul     ++ (decide secondaryop (head(tail xs)))
                               Div      -> (decide secondaryop (head xs)) ++ p Div     ++ (decide secondaryop (head(tail xs)))
                               Mod      -> (decide secondaryop (head xs)) ++ p Mod     ++ (decide secondaryop (head(tail xs)))
                               Not      -> p Not ++ (decide not' (head xs))

ppProgram :: Program -> (Int -> String)
ppProgram p = (\n -> ppProgram' p n)

ppProgram' :: Program -> Int -> String
ppProgram' (i := e)         n = (spaces n) ++ i ++ " := " ++ (ppExpr e) ++ ";\n"
ppProgram' (Block l)        n = (spaces n) ++ "{\n" ++ (seq' l (n+1)) ++ "}\n"  
ppProgram' (While e p)      n = (spaces n) ++ "while (" ++ (ppExpr e) ++ ")\n" ++ (ppProgram' p (n+1))
ppProgram' (If e p)         n = (spaces n) ++ "if (" ++ (ppExpr e) ++ ")\n" ++ (ppProgram' p (n+1))
ppProgram' (IfElse e p1 p2) n = (ppProgram' (If e p1) n) ++ (spaces n) ++ "else\n" ++ (ppProgram' p2 (n+1))
ppProgram' (Read i)         n = (spaces n) ++ "read " ++ i ++ ";\n"
ppProgram' (Write e)        n = (spaces n) ++ "write (" ++ (ppExpr e) ++ ");\n"
ppProgram' (Print s)        n = (spaces n) ++ "print \"" ++ s ++ "\";\n"

seq' :: [Program] -> Int -> String
seq' [] n = []
seq' (x:xs) n = (ppProgram' x n) ++ (seq' xs n)

-----------------------------UTILS FOR ppExpr

e = (Op Not [Var "x",Constant 0])

--year % 4 == 0 && year % 100 != 0 || year % 400 == 0

e1 = Op Or [(Op And [(Op Eq [(Op Mod [Var "year", Constant 4]), Constant 0]), Op Less [(Op Mod [Var "year", Constant 100]), Constant 0]]), (Op Eq [(Op Mod [Var "year", Constant 400]), Constant 0])]


decide :: (Expr -> Bool) -> Expr -> String
decide f e | f e == True = "(" ++ (ppExpr e) ++ ")"
           | otherwise   = ppExpr e 

p :: OpName -> String
p Or      = " || "
p And     = " && "
p Eq      = " == "
p Leq     = " <= "
p Less    = " < " 
p Geq     = " >= "
p Greater = " > "
p Add     = " + "
p Sub     = " - "
p Mul     = " * "
p Div     = " / "
p Mod     = " % "
p Not     = " ! "

or' :: Expr -> Bool
or' (Constant n) = False
or' (Var i)      = False
or' (Op Or _)    = False
or' (Op And _)   = False
or' (Op Not _)   = False
or' _            = True

and' :: Expr -> Bool
and' (Constant n) = False
and' (Var i)      = False
and' (Op And _)   = False
and' (Op Not _)   = False
and' _            = True

-- == <= < >= >
eq' :: Expr -> Bool
eq' (Constant n) = False
eq' (Var i)      = False
eq' (Op Not _)   = False
eq' (Op Add _)   = False
eq' (Op Sub _)   = False
eq' (Op Mul _)   = False
eq' (Op Div _)   = False
eq' (Op Mod _)   = False
eq' _            = True

-- + -
primaryop :: Expr -> Bool
primaryop (Constant n) = False
primaryop (Var i)      = False
primaryop (Op Add _)   = False
primaryop (Op Sub _)   = False
primaryop (Op Mul _)   = False
primaryop (Op Div _)   = False
primaryop (Op Mod _)   = False
primaryop (Op Not _)   = False
primaryop _            = True

-- * / %
secondaryop :: Expr -> Bool
secondaryop (Constant n) = False
secondaryop (Var i)      = False
secondaryop (Op Mul _)   = False
secondaryop (Op Div _)   = False
secondaryop (Op Mod _)   = False
secondaryop (Op Not _)   = False
secondaryop _            = True

not' :: Expr -> Bool
not' (Constant n) = False
not' (Var i)      = False
not' (Op Not _)   = False
not' _            = True






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

a = (ppProgram fibonacciProgram) 0



