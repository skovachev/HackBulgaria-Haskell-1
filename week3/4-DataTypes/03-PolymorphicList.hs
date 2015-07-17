data List a = Constructor a (List a) | Empty
    deriving (Show)

fromList :: [a] -> List a
fromList (x:xs) = Constructor x (fromList xs)
fromList [] = Empty

toList   :: List a -> [a]
toList (Constructor x xs) = x : (toList xs)
toList Empty = []

mapList :: (a -> a) -> List a -> List a
mapList func (Constructor x xs) = Constructor (func x) (mapList func xs)

filterList :: (a -> Bool) -> List a -> List a
filterList func (Constructor x xs) | canPass = Constructor x (filterList func xs)
                                      | otherwise = filterList func xs
                                        where canPass = func x
filterList func Empty = Empty

foldList :: (t -> a -> t) -> t -> List a -> t
foldList func startValue (Constructor x xs) = foldList func (func startValue x) xs
foldList func startValue Empty = startValue

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _     = Nothing

safeTail :: [t] -> Maybe [t]
safeTail (_:x) = Just x
safeTail _     = Nothing

safeNth :: Int -> [a] -> Maybe a
safeNth 0 list = safeHead list
safeNth pos (x:xs) | pos < 0 = Nothing
                   | otherwise = safeNth (pos-1) xs