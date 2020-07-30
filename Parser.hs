module Parser where

data ParseTree = ParseAtom {getStr :: String} |
    ParseList {getList :: [ParseTree]}
    deriving (Show)

isParseAtom (ParseAtom _) = True
isParseAtom _ = False

isParseList (ParseList _) = True
isParseList _ = False

isWspc c = elem c wspcSet
    where wspcSet = " \n\r\t"

isBrkt c = elem c brktSet
    where brktSet = "()[]"

ignoreWspc x = dropWhile isWspc x

parseAtom withWspc =
        let (str, rest) = break (\c -> isWspc c || isBrkt c) x
        in (ParseAtom str, rest)
    where x = ignoreWspc withWspc

parseList withWspc =
    if (\x -> x=='(' || x=='[') $ head x then
        let
            (listHead, tmp) = parseList $ tail x
            (ParseList listTail, rest) = parseList tmp
        in (ParseList $ listHead:listTail, rest)
    else if (\x -> x==')' || x==']') $ head x then (ParseList [], tail x)
    else
        let
            (head, tmp) = parseAtom x
            (ParseList tail, rest) = parseList tmp
        in (ParseList $ head:tail, rest)
    where x = ignoreWspc withWspc

parseProgramList withWspc = case x of
    "" -> []
    ('(':_) ->
        let (tree, rest) = parseList $ tail x
        in tree:(parseProgramList rest)
    ('[':_) ->
        let (tree, rest) = parseList $ tail x
        in tree:(parseProgramList rest)
    _ ->
        let (atom, rest) = parseAtom x
        in atom:(parseProgramList rest)
    where x = ignoreWspc withWspc

parseProgram x = case progList of
    [a] -> a
    _ -> ParseList progList
    where progList = parseProgramList x
