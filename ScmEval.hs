module ScmEval where
import ScmVal

import Data.IORef
import Control.Monad
import Data.Maybe

convListToScm [x] = x
convListToScm x@(_:_) = ListScmVal x

scmEval monad (ListScmVal [ScmFree "if", testExpr, thenExpr, elseExpr]) =
    do
        term <- scmEval monad testExpr
        case term of
            ScmInteger x -> scmEval monad $ if x/=0 then thenExpr else elseExpr
            ScmDouble x -> scmEval monad $ if x/=0.0 then thenExpr else elseExpr
            ScmChar x -> scmEval monad $ if x/='\NUL' then thenExpr else elseExpr
            ScmBool x -> scmEval monad $ if x then thenExpr else elseExpr
            ScmPair _ _ -> scmEval monad thenExpr
            ScmEmptyList -> scmEval monad elseExpr

scmEval monad (ListScmVal ((ScmFree "cond"):(ListScmVal [ScmFree "else", thenExpr]):_)) =
    scmEval monad thenExpr

scmEval monad (ListScmVal ((ScmFree "cond"):(ListScmVal [testExpr, thenExpr]):rest)) =
    scmEval monad $ ListScmVal $ [ScmFree "if", testExpr, thenExpr, condRest]
    where condRest = ListScmVal $ (ScmFree "cond"):rest

scmEval monad (ListScmVal [ScmFree "lambda", ListScmVal args, body]) =
    return $ ScmLambda monad (map getStr args) body

scmEval monad (ListScmVal [ScmFree "define", ScmFree str, term]) =
    do
        env <- readIORef monad
        val <- scmEval monad term
        writeIORef monad (envInsert str val env)
        return val

scmEval monad (ScmFree name) =
    do
        env <- readIORef monad
        return $ fromJust $ envFind name env

scmEval monad (ListScmVal ((ScmFree "define"):(ListScmVal (fun:args)):terms)) =
    let
        lambdaTerm = ListScmVal [ScmFree "lambda", ListScmVal args, convListToScm terms]
        defineTerm = ListScmVal [ScmFree "define", fun, lambdaTerm]
    in scmEval monad defineTerm

scmEval monad (ListScmVal (d@(ListScmVal ((ScmFree "define"):_)):terms)) =
    do
        scmEval monad d
        scmEval monad $ convListToScm terms

scmEval monad (ListScmVal ((ScmFree funStr):terms)) =
    do
        env <- readIORef monad
        case fromJust $ envFind funStr env of
            (ScmBuiltinFun f) -> do
                args <- mapM (scmEval monad) terms
                return $ f args
            l@(ScmLambda _ _ _) ->
                scmEval monad (ListScmVal $ l:terms)

scmEval monad (ListScmVal (fun:terms)) =
    do
        (ScmLambda monadParentEnv strs body) <- scmEval monad fun
        parentEnv <- readIORef monadParentEnv
        args <- mapM  (scmEval monad) terms
        let assocList = zip strs args
        newMonad <- newIORef (envCreate assocList parentEnv)
        scmEval newMonad body

scmEval _ atom = return atom
