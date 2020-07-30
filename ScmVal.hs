module ScmVal where
import Parser
import Map

import Data.Char
import Data.Maybe
import Text.Read
import Data.IORef

data ScmEnv = EmptyEnv | ScmEnv {getMap :: Map String ScmVal, getParent :: ScmEnv}
    deriving (Show)

envCreateNew x = envCreate x EmptyEnv
envCreate x parentEnv = ScmEnv (mapCreate x) parentEnv

envInsert name val (ScmEnv map parent) = ScmEnv (mapInsert name val map) parent

envFind x EmptyEnv = Nothing
envFind x (ScmEnv map parent) = case mapFind x map of
    Nothing -> envFind x parent
    j@(Just _) -> j

data ScmVal =  ScmEmptyList |
    ScmInteger {getInteger :: Integer} |
    ScmDouble {getDouble :: Double} |
    ScmChar {getChar :: Char} |
    ScmBool {getBool :: Bool} |
    ScmPair {getFst :: ScmVal, getSnd :: ScmVal} |
    ScmFree {getStr :: String} |
    ScmBuiltinFun {getFun :: [ScmVal] -> ScmVal} |
    ScmLambda {getEnv :: IORef ScmEnv, getArgs :: [String], getBody :: ScmVal} |
    ListScmVal {getList :: [ScmVal]}

scmShow ScmEmptyList = "'()"

scmShow (ScmInteger x) = show x
scmShow (ScmDouble x) = show x

scmShow (ScmChar x) = "#\\" ++ [x]
scmShow (ScmBool True) = "#t"
scmShow (ScmBool False) = "#f"

scmShow (ScmPair x y@(ScmPair _ _)) = "(" ++ (scmShow x) ++ " " ++ (tail $ scmShow y)
scmShow (ScmPair x ScmEmptyList) = "(" ++ (scmShow x) ++ ")"
scmShow (ScmPair x y) = "(" ++ (scmShow x) ++ " . " ++ (scmShow y) ++ ")"

scmShow (ScmFree name) = name

scmShow (ScmBuiltinFun _) = "#<procedure>"
scmShow (ScmLambda _ _ _) = "#<procedure>"

scmShow (ListScmVal []) = "()"
scmShow (ListScmVal l) = "(" ++ insideBrkt ++ ")"
    where insideBrkt = tail $ concat $ map ((' ':) . scmShow) l

instance Show ScmVal where
    show = scmShow

scmParseInteger x = if isJust maybeInt
    then Just $ ScmInteger $ fromJust maybeInt
    else Nothing
    where maybeInt = readMaybe x :: Maybe Integer

scmParseDouble x = if isJust maybeFlp
    then Just $ ScmDouble $ fromJust maybeFlp
    else Nothing
    where maybeFlp = readMaybe x :: Maybe Double


readHexMaybe [] res = Just res
readHexMaybe (x:xs) res =
    if not $ isHexDigit x then Nothing
    else readHexMaybe xs $ res*16 + (digitToInt x)

scmParseChar ("#\\nul") = Just $ ScmChar '\NUL'
scmParseChar (['#', '\\', 'u', a, b, c, d]) = case readHexMaybe [a, b, c, d] 0 of
    Just hex -> Just $ ScmChar $ (toEnum hex :: Char)
    Nothing -> Nothing
scmParseChar _ = Nothing

scmParseBool (['#', 't']) = Just $ ScmBool True
scmParseBool (['#', 'f']) = Just $ ScmBool False
scmParseBool _ = Nothing

scmParseDefault x = Just $ ScmFree x

scmParseAtom x =
    let
        for (p:ps) = case p x of
            Just scmVal -> scmVal
            Nothing -> for ps
    in for parsers
    where parsers = [scmParseInteger, scmParseDouble, scmParseChar,
            scmParseBool, scmParseDefault]

scmRead x = transform $ parseProgram x
    where
        transform (ParseAtom x) = scmParseAtom x
        transform (ParseList l) = ListScmVal $ map transform l
