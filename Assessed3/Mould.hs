{-# LANGUAGE Safe #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE CPP  #-}
{-

This module will import your solutions, check that they have the right type,
and re-export your solutions.

-}

module Mould
    (program, expr, expr1, expr2, expr3, expr4, expr5, expr6, expr7, orOp, andOp, eqOp, compOp, addOp, mulOp, notOp, binExpr, parseOp, constant, keywords, identif, parseProgram, run, runIO', ppProgram, ppExpr, optExpr, optProgram, optProgram')
where

import Data.Typeable (Typeable)
import Control.DeepSeq (NFData)

import safe AbstractSyntax
import safe IOPrime
import safe Parsing
import safe Unsolved

import safe qualified Parser as StudentParser
import safe qualified Interpreter as StudentInterp
import safe qualified PrettyPrinting as StudentPP
import safe qualified Optimization as StudentOpt

deriving instance Typeable OpName
deriving instance Typeable Expr
deriving instance Typeable Program

type Storage = Identifier -> Integer
type OptStorage = Identifier -> Maybe Integer
emptyOptStorage :: OptStorage
emptyOptStorage i = Nothing



program :: Parser Program
program = StudentParser.program
expr  :: Parser Expr
expr = StudentParser.expr
expr1 :: Parser Expr
expr1 = StudentParser.expr1
expr2 :: Parser Expr
expr2 = StudentParser.expr2
expr3 :: Parser Expr
expr3 = StudentParser.expr3
expr4 :: Parser Expr
expr4 = StudentParser.expr4
expr5 :: Parser Expr
expr5 = StudentParser.expr5
expr6 :: Parser Expr
expr6 = StudentParser.expr6
expr7 :: Parser Expr
expr7 = StudentParser.expr7
orOp   :: Parser ([Expr] -> Expr)
orOp = StudentParser.orOp
andOp  :: Parser ([Expr] -> Expr)
andOp = StudentParser.andOp
eqOp   :: Parser ([Expr] -> Expr)
eqOp = StudentParser.eqOp
compOp :: Parser ([Expr] -> Expr)
compOp = StudentParser.compOp
addOp  :: Parser ([Expr] -> Expr)
addOp = StudentParser.addOp
mulOp  :: Parser ([Expr] -> Expr)
mulOp = StudentParser.mulOp
notOp  :: Parser ([Expr] -> Expr)
notOp = StudentParser.notOp
binExpr :: Parser e -> Parser ([e] -> e) -> Parser e -> Parser e
binExpr = StudentParser.binExpr
parseOp :: String -> OpName -> Parser ([Expr] -> Expr)
parseOp = StudentParser.parseOp
constant :: Parser Expr
constant = StudentParser.constant
keywords :: [String]
keywords = StudentParser.keywords
identif :: Parser String
identif = StudentParser.identif
parseProgram :: String -> Program
parseProgram = StudentParser.parseProgram

run :: Program -> Storage -> IO' Storage
run = StudentInterp.run
runIO' :: Program -> IO'()
runIO' = StudentInterp.runIO'

ppProgram :: Program -> (Int -> String)
ppProgram = StudentPP.ppProgram
ppExpr :: Expr -> String
ppExpr = StudentPP.ppExpr

optExpr :: OptStorage -> Expr -> Expr
optExpr = StudentOpt.optExpr
optProgram  :: Program -> OptStorage -> (Program, OptStorage)
optProgram = StudentOpt.optProgram
optProgram' :: Program -> Program
optProgram' p = fst(optProgram p emptyOptStorage)
