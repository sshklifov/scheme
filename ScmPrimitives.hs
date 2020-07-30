module ScmPrimitives where
import Map
import ScmVal

primitives = envCreateNew $ map (\(x, y) -> (x, ScmBuiltinFun y))
    [("+", scmSum), ("-", scmSub), ("*", scmMult), ("/", scmDiv),
    ("remainder", scmRemainder), ("quotient", scmQuotient),
    ("max", scmMax), ("min", scmMin), ("gcd", scmGcd), ("lcm", scmLcm),
    ("floor", scmFloor), ("ceiling", scmCeiling), ("scmRound", scmRound),
    ("exp", scmExp), ("log", scmLog), ("sin", scmSin), ("cos", scmCos),
    ("tan", scmTan), ("asin", scmAsin), ("acos", scmAcos), ("atan", scmAtan),
    ("expt", scmExpt), ("sqrt", scmSqrt), ("<", scmLt), (">", scmGt),
    ("=", scmEq), ("<=", scmLe), (">=", scmGe), ("boolean?", scmBooleanQ),
    ("number?", scmNumberQ), ("char?", scmCharQ), ("pair?", scmPairQ),
    ("null?", scmNullQ), ("list?", scmListQ), ("procedure?", scmProcedureQ),
    ("equal?", scmEqualQ), ("cons", scmCons), ("car", scmCar), ("cdr", scmCdr),
    ("list", scmList)]

scmToDouble (ScmInteger x) = ScmDouble $ fromIntegral x
scmToDouble x@(ScmDouble _) = x

scmBinOp opInt opFlp (ScmInteger x) (ScmInteger y) = opInt x y
scmBinOp opInt opFlp x y = opFlp (t x) (t y)
    where t = getDouble . scmToDouble

scmBinOpAsNum opInt opFlp (ScmInteger x) (ScmInteger y) = ScmInteger $ opInt x y
scmBinOpAsNum opInt opFlp x y = ScmDouble $ opFlp (t x) (t y)
    where t = getDouble . scmToDouble

scmInjectDouble f (ScmDouble x) = ScmDouble $ f x
scmInjectInteger f (ScmInteger x) = ScmInteger $ f x

scmSum l = foldl1 (scmBinOpAsNum (+) (+)) l
scmSub l = foldl1 (scmBinOpAsNum (-) (-)) l
scmMult l = foldl1 (scmBinOpAsNum (*) (*)) l
scmDiv l = foldl1 (scmBinOpAsNum (div) (/)) l

scmRemainder [(ScmInteger x), (ScmInteger y)] = ScmInteger $ x `mod` y
scmQuotient [(ScmInteger x), (ScmInteger y)] = ScmInteger $ x `div` y

scmMax l = foldl1 (scmBinOpAsNum max max) l
scmMin l = foldl1 (scmBinOpAsNum min min) l

scmGcd [x] = x
scmGcd (x:xs) = scmInjectInteger (gcd (getInteger x)) $ scmGcd xs

scmLcm [x] = x
scmLcm (x:xs) = scmInjectInteger (lcm (getInteger x)) $ scmLcm xs

scmFloor [x] = ScmInteger $ floor $ getDouble x
scmCeiling [x] = ScmInteger $ ceiling $ getDouble x
scmRound [x] = ScmInteger $ round $ getDouble x

scmExp [x] = ScmDouble $ exp $ getDouble x
scmLog [x] = ScmDouble $ log $ getDouble x
scmSin [x] = ScmDouble $ sin $ getDouble x
scmCos [x] = ScmDouble $ cos $ getDouble x
scmTan [x] = ScmDouble $ tan $ getDouble x
scmAsin [x] = ScmDouble $ asin $ getDouble x
scmAcos [x] = ScmDouble $ acos $ getDouble x
scmAtan [x] = ScmDouble $ atan $ getDouble x
scmExpt [x, y] = scmBinOpAsNum (^) (**) x y
scmSqrt [x] = ScmDouble $ sqrt $ getDouble x

scmLt [x, y] = ScmBool $ scmBinOp (<) (<) x y
scmGt [x, y] = ScmBool $ scmBinOp (>) (>) x y
scmEq [x, y] = ScmBool $ scmBinOp (==) (==) x y
scmLe [x, y] = ScmBool $ scmBinOp (<=) (<=) x y
scmGe [x, y] = ScmBool $ scmBinOp (>=) (>=) x y

scmBooleanQ [(ScmBool _)] = ScmBool True
scmBooleanQ [_] = ScmBool False

scmNumberQ [(ScmInteger _)] = ScmBool True
scmNumberQ [_] = ScmBool False

scmCharQ [(ScmChar _)] = ScmBool True
scmCharQ [_] = ScmBool False

scmPairQ [ScmPair _ _] = ScmBool True
scmPairQ _ = ScmBool False

scmNullQ [ScmEmptyList] = ScmBool True
scmNullQ _ = ScmBool False

scmListQ [ScmEmptyList] = ScmBool True
scmListQ [ScmPair _ x] = scmListQ [x]
scmListQ _ = ScmBool False

scmProcedureQ [ScmBuiltinFun _] = ScmBool True
scmProcedureQ [ScmLambda _ _ _] = ScmBool True
scmProcedureQ _ = ScmBool False

scmEqualQ [ScmEmptyList, ScmEmptyList] = ScmBool True
scmEqualQ [ScmInteger x, ScmInteger y] = ScmBool $ x==y
scmEqualQ [ScmDouble x, ScmDouble y] = ScmBool $ x==y
scmEqualQ [ScmChar x, ScmChar y] = ScmBool $ x==y
scmEqualQ [ScmBool x, ScmBool y] = ScmBool $ x==y

scmEqualQ [ScmPair xfst xsnd, ScmPair yfst ysnd] = if getBool $ scmEqualQ [xfst, yfst]
    then scmEqualQ [xsnd, ysnd]
    else ScmBool False

scmEqualQ [_, _] = ScmBool False

scmCons [x, y] = ScmPair x y
scmCar [ScmPair x y] = x
scmCdr [ScmPair x y] = y

scmList [] = ScmEmptyList
scmList (x:xs) = ScmPair x $ scmList xs
