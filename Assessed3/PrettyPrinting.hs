module PrettyPrinting where


import Unsolved
import AbstractSyntax

spaces n = take n (repeat ' ')

ppExpr :: Expr -> String
ppExpr (Constant n) = show n
ppExpr (Var i) = i
ppExpr (Op n []) = undefined
ppExpr (Op n (xs)) = case n of Or       -> (decide or' (head xs))           ++ p Or      ++ (decide or' (head(tail xs)))
                               And      -> (decide and' (head xs))          ++ p And     ++ (decide and' (head(tail xs)))
                               Eq       -> (decide eq' (head xs))           ++ p Eq      ++ (decide eq'' (head(tail xs)))
                               Leq      -> (decide comp' (head xs))         ++ p Leq     ++ (decide comp'' (head(tail xs)))
                               Less     -> (decide comp' (head xs))         ++ p Less    ++ (decide comp'' (head(tail xs)))
                               Geq      -> (decide comp' (head xs))         ++ p Geq     ++ (decide comp'' (head(tail xs)))
                               Greater  -> (decide comp' (head xs))         ++ p Greater ++ (decide comp'' (head(tail xs)))
                               Add      -> (decide add' (head xs))          ++ p Add     ++ (decide add'' (head(tail xs)))
                               Sub      -> (decide sub' (head xs))          ++ p Sub     ++ (decide sub'' (head(tail xs)))
                               Mul      -> (decide mult' (head xs))         ++ p Mul     ++ (decide mult'' (head(tail xs)))
                               Div      -> (decide secondaryop (head xs))   ++ p Div     ++ (decide secondaryop' (head(tail xs)))
                               Mod      -> (decide secondaryop (head xs))   ++ p Mod     ++ (decide secondaryop' (head(tail xs)))
                               Not      -> p Not ++ (decide not' (head xs))

ppProgram :: Program -> (Int -> String)
ppProgram p = (\n -> ppProgram' p n)

ppProgram' :: Program -> Int -> String
ppProgram' (i := e)         n = (spaces n) ++ i ++ " := " ++ (ppExpr e) ++ ";\n"
ppProgram' (Block l)        n = (spaces n) ++ "{\n" ++ (seq' l (n+2)) ++ (spaces n) ++ "}\n"  
ppProgram' (While e p)      n = (spaces n) ++ "while (" ++ (ppExpr e) ++ ")\n" ++ (ppProgram' p (n+2))
ppProgram' (If e p)         n = (spaces n) ++ "if (" ++ (ppExpr e) ++ ")\n" ++ (ppProgram' p (n+2))
ppProgram' (IfElse e p1 p2) n = (ppProgram' (If e p1) n) ++ (spaces n) ++ "else\n" ++ (ppProgram' p2 (n+2))
ppProgram' (Read i)         n = (spaces n) ++ "read " ++ i ++ ";\n"
ppProgram' (Write e)        n = (spaces n) ++ "write " ++ (ppExpr e) ++ ";\n"
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
p Not     = "! "

--associative operators
or' :: Expr -> Bool
or' _ = False

and' :: Expr -> Bool
and' (Op Or _) = True
and' _         = False    

add' :: Expr -> Bool
add' (Op Or _)      = True
add' (Op And _)     = True
add' (Op Eq _)      = True
add' (Op Leq _)     = True
add' (Op Less _)    = True
add' (Op Geq _)     = True
add' (Op Greater _) = True
add' _              = False

add'' :: Expr -> Bool
add'' (Op Sub _) = True
add'' x = add' x

mult' :: Expr -> Bool
mult' (Op Or _)      = True
mult' (Op And _)     = True
mult' (Op Eq _)      = True
mult' (Op Leq _)     = True
mult' (Op Less _)    = True
mult' (Op Geq _)     = True
mult' (Op Greater _) = True
mult' (Op Add _)     = True
mult' (Op Sub _)     = True
mult' _              = False

mult'' :: Expr -> Bool
mult'' (Op Div _) = True
mult'' (Op Mod _) = True
mult'' x = mult' x


--non associative
eq' :: Expr -> Bool
eq' (Op Or _)  = True
eq' (Op And _) = True
eq' _          = False

eq'' :: Expr -> Bool
eq'' (Op Eq _) = True
eq'' x = eq' x 

-- <= < >= >
comp' :: Expr -> Bool
comp' (Op Or _)  = True
comp' (Op And _) = True
comp' (Op Eq _)  = True
comp' _          = False 

comp'' :: Expr -> Bool
comp'' (Op Leq _) = True
comp'' (Op Less _) = True
comp'' (Op Geq _) = True
comp'' (Op Greater _) = True
comp'' x = comp' x

-- -
sub' :: Expr -> Bool
sub' (Op Or _)      = True
sub' (Op And _)     = True
sub' (Op Eq _)      = True
sub' (Op Leq _)     = True
sub' (Op Less _)    = True
sub' (Op Geq _)     = True
sub' (Op Greater _) = True
sub' _              = False

sub'' :: Expr -> Bool
sub'' (Op Add _) = True
sub'' (Op Sub _) = True
sub'' x = sub' x

-- / %
secondaryop :: Expr -> Bool
secondaryop (Op Or _)      = True
secondaryop (Op And _)     = True
secondaryop (Op Eq _)      = True
secondaryop (Op Leq _)     = True
secondaryop (Op Less _)    = True
secondaryop (Op Geq _)     = True
secondaryop (Op Greater _) = True
secondaryop (Op Add _)     = True
secondaryop (Op Sub _)     = True
secondaryop _              = False

secondaryop' :: Expr -> Bool
secondaryop' (Op Mul _) = True
secondaryop' (Op Div _) = True
secondaryop' (Op Mod _) = True
secondaryop' x = secondaryop x

not' :: Expr -> Bool
not' (Constant n) = False
not' (Var i)      = False
not' (Op Not _)   = False
not' _            = True



