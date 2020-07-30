module Map where

data Map a b = EmptyTree |
    BinaryTree {getLeft :: Map a b, getRight :: Map a b, getRoot :: (a, b)}
    deriving (Show)

merge less [] y = y
merge less x [] = x
merge less (x:xs) (y:ys)
    | less x y = (:) x $ merge less xs (y:ys)
    | otherwise = (:) y $ merge less (x:xs) ys

mergeSort _ [] = []
mergeSort _ [x] = [x]
mergeSort less x =
    let
        n = length x
        (left, right) = splitAt (n `div` 2) x
    in merge less (mergeSort less left) (mergeSort less right)

mapCreate x = helper $ mergeSort (\(leftK, _) (rightK, _) -> leftK<rightK) x
    where
        helper [] = EmptyTree
        helper x =
            let
                n = (length x) - 1
                (left, midConsRight) = splitAt (n `div` 2) x
            in BinaryTree (helper left) (helper $ tail midConsRight) (head midConsRight)

mapInorder EmptyTree = []
mapInorder (BinaryTree left right root) =
    (++) (mapInorder left) $ root:(mapInorder right)

mapInsert k v EmptyTree = BinaryTree EmptyTree EmptyTree (k, v)
mapInsert k v (BinaryTree left right root@(rootKey, rootVal))
    | k < rootKey = BinaryTree (mapInsert k v left) right root
    | k > rootKey = BinaryTree left (mapInsert k v right) root
    | otherwise = BinaryTree left right (k, v)


mapFind k EmptyTree = Nothing
mapFind k (BinaryTree left right root@(rootKey, rootVal))
    | k < rootKey = mapFind k left
    | k > rootKey = mapFind k right
    | otherwise = Just rootVal

mapHas k t = case (mapFind k t) of
    Nothing -> False
    Just _ -> True
