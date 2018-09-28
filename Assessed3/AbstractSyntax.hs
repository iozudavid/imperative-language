{-# LANGUAGE Safe #-} 

-- This file need not be submitted, and if it is submitted it will be
-- ignored, as we will use the original version when marking.
-- Do not modify it.

module AbstractSyntax where

type Identifier = String

data OpName  = Or                                 --  ||
             | And                                --  &&
             | Eq                                 --  ==
             | Leq | Less | Geq | Greater         --  <=  <  >=  >
             | Add | Sub                          --  +  -
             | Mul | Div | Mod                    --  *  /  %
             | Not                                --  !
             deriving (Eq,Show)

data Expr    = Constant Integer
             | Var Identifier
             | Op OpName [Expr]
            deriving (Eq,Show)

data Program = Identifier := Expr
             | Block [Program]
             | While Expr Program
             | If Expr Program
             | IfElse Expr Program Program
             | Read Identifier
             | Write Expr
             | Print String
             deriving (Eq,Show)

