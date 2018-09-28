{-# LANGUAGE Safe #-} 

module Main where

import System.Environment

import AbstractSyntax
import Parser
import Interpreter
import IOPrime

main :: IO()
main =
  do
    args <- getArgs
    if length args == 1
       then
         do
           sourceCode <- readFile (args !! 0)
           translate (run (parseProgram sourceCode) emptyStorage)
           return ()
       else
           putStrLn "Usage: runhaskell RunIO.hs <filename>"
