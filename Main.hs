module Main where

import ScmVal
import ScmEval
import ScmPrimitives

import Data.IORef
import System.Environment
import System.IO
import Control.Monad

replReadFromStdin str = putStr str >> hFlush stdout >> getLine

replReadFromFile str =
    do
        file <- openFile str ReadMode
        str <- hGetContents file
        return $ tail $ dropWhile (/= '\n') str

replEvalPrint monad str =
    do
        scmval <- scmEval monad $ scmRead str
        putStrLn $ show scmval

replCycle doRead doEvalPrint =
    do
        str <- doRead
        doEvalPrint str

main =
    do
        monad <- newIORef primitives
        args <- System.Environment.getArgs
        case length args of
            0 -> do forever $ replCycle (replReadFromStdin "> ") (replEvalPrint monad)
            1 -> do replCycle (replReadFromFile $ args !! 0) (replEvalPrint monad)
