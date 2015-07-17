data IntList = Constructor Int IntList | Empty
    deriving (Show)

fromList :: [Int] -> IntList
fromList (x:xs) = Constructor x (fromList xs)
fromList [] = Empty

toList   :: IntList -> [Int]
toList (Constructor x xs) = x : (toList xs)
toList Empty = []

mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList func (Constructor x xs) = Constructor (func x) (mapIntList func xs)

filterIntList :: (Int -> Bool) -> IntList -> IntList
filterIntList func (Constructor x xs) | canPass = Constructor x (filterIntList func xs)
                                      | otherwise = filterIntList func xs
                                        where canPass = func x
filterIntList func Empty = Empty

foldIntList :: (t -> Int -> t) -> t -> IntList -> t
foldIntList func startValue (Constructor x xs) = foldIntList func (func startValue x) xs
foldIntList func startValue Empty = startValue