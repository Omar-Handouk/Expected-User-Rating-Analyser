--Author: Omar Ihab Hamed Handouk
--Title: Recommender System
--Date started: 28 / 4 / 2018

data UI = U String | I String deriving (Show, Eq)
data Rating c = NoRating | R c deriving (Show, Eq)

dis :: Eq a => [a] -> [a]
dis [] = []
dis (h : t) = if elem h t then dis t else h : (dis t)

fromRatingsToItems :: Eq t => [(a, t, b)] -> [t]
fromRatingsToItemsHelper :: Eq t => [(a, t, b)] -> [t] -> [t]
fromRatingsToItems list = fromRatingsToItemsHelper list []
fromRatingsToItemsHelper [] acc = acc
fromRatingsToItemsHelper ((a, t, b) : xs) acc | elem t acc = x acc
                                              | otherwise = x (acc ++ [t])
                                              where 
                                                x = fromRatingsToItemsHelper xs

fromRatingsToUsers :: Eq a => [(a, b, c)] -> [a]
fromRatingsToUsersHelper :: Eq t => [(t, a, b)] -> [t] -> [t]
fromRatingsToUsers list = fromRatingsToUsersHelper list []
fromRatingsToUsersHelper [] acc = acc
fromRatingsToUsersHelper ((t, a, b) : xs) acc | elem t acc = x acc
                                              | otherwise = x (acc ++ [t])
                                              where
                                                x = fromRatingsToUsersHelper xs

hasRating :: (Eq t1, Eq t2) => t1 -> t2 -> [(t1, t2, a)] -> Bool
hasRating a b [] = False
hasRating a b ((x, y, z) : xs) | a == x && b == y = True
                               | otherwise = hasRating a b xs

getRating :: (Eq t2, Eq t1) => t1 -> t2 -> [(t1, t2, p)] -> p
searchRating :: (Eq t2, Eq t1) => t1 -> t2 -> [(t1, t2, p)] -> p
getRating a b list | hasRating a b list = searchRating a b list
                   | otherwise = error "No Given Rating" --Should be replaced with putStr, although I need to check
searchRating a b ((x, y, z) : xs) | a == x && b == y = z
                                  | otherwise = searchRating a b xs

formMatrixUser :: (Eq t1, Eq t2, Fractional c) => t1 -> [t2] -> [(t1,t2,c)] -> [Rating c]                                  
formMatrixUser _ [] _ = []
formMatrixUser a (x : xs) list | hasRating a x list = (R (getRating a x list)) : q
                               | otherwise = NoRating : q
                               where q = formMatrixUser a xs list

formMatrix :: (Eq t1, Eq t2, Fractional c) => [t1] -> [t2] -> [(t1, t2, c)] -> [[Rating c]]                               
formMatrix [] _ _ = []
formMatrix (x : xs) a b = formMatrixUser x a b : formMatrix xs a b

numberRatingsGivenItem :: (Num p, Eq c) => Int -> [[Rating c]] -> p
checkRating :: (Num p, Eq c) => Int -> [Rating c] -> p
numberRatingsGivenItem _ [] = 0
numberRatingsGivenItem a (x : xs) = (checkRating a x) + (numberRatingsGivenItem a xs)
checkRating a x = if (x !! a) == NoRating then 0 else 1

differeneRatings :: (Eq a, Fractional a) => Rating a -> Rating a -> a
getDifference :: Fractional a => Rating a -> Rating a -> a
differeneRatings a b | a == NoRating || b == NoRating = 0.0
                     | otherwise = getDifference a b
getDifference (R a) (R b) = a - b

matrixPairs :: (Num a, Eq a) => a -> [(a, a)]
matrixPairsHelper :: (Num a, Eq a) => a -> a -> a -> [(a, a)]
matrixPairs a = matrixPairsHelper 0 0 a
matrixPairsHelper a b c | a == c = []
                        | b == c = matrixPairsHelper (a + 1) 0 c
                        | otherwise = (a, b) : matrixPairsHelper a (b + 1) c

dMatrix :: (Eq a, Fractional a) => [[Rating a]] -> [a]
dMatrixHelper :: (Eq a, Fractional a) => [(Int, Int)] -> [[Rating a]] -> [a]
sumDiff :: (Fractional p, Eq p) => (Int, Int) -> [[Rating p]] -> p
dMatrix (x : xs) = dMatrixHelper (matrixPairs (length x)) (x : xs)
dMatrixHelper [] _ = []
dMatrixHelper (x : xs) list = (sumDiff x list) : (dMatrixHelper xs list)
sumDiff _ [] = 0.0
sumDiff (a, b) (x : xs) = (differeneRatings (x !! a) (x !! b)) + (sumDiff (a, b) xs) --Dab = b - a not a - b

freqMatrix :: (Eq a, Num b) => [[Rating a]] -> [b]
freqMatrixHelper :: (Eq a, Num b) => [(Int, Int)] -> [[Rating a]] -> [b]
getFreq :: (Eq a, Num b) => (Int, Int) -> [[Rating a]] -> b
freqMatrix (x : xs) = freqMatrixHelper (matrixPairs (length x)) (x : xs)
freqMatrixHelper [] _ = []
freqMatrixHelper (x : xs) list = (getFreq x list) : (freqMatrixHelper xs list)
getFreq _ [] = 0
getFreq (a, b) (x : xs) | (x !! a) /= NoRating && (x !! b) /= NoRating = 1 + q
                        | otherwise =  q
                        where
                          q =  getFreq (a, b) xs

diffFreqMatrix :: (Eq a, Fractional a) => [[Rating a]] -> [a]
diffFreqMatrixHelper :: Fractional a => [a] -> [a] -> [a]                    
diffFreqMatrix list = diffFreqMatrixHelper (dMatrix list) (freqMatrix list)
diffFreqMatrixHelper [] _ = []
diffFreqMatrixHelper (a : b) (c : d) = (a / c) : diffFreqMatrixHelper b d

--predict/3 (ratings matrix, user Index, item Index)
--To cases to handle: 1) A review for the product is already found, 2) probabilistic prediction for an item's expected rating.
predict :: (Eq t1, Eq t2, Fractional c, Eq c) => [(t1, t2, c)] -> Int -> Int -> c
predictHelper :: (Fractional a, Eq a) => [a] -> [Rating a] -> Int -> Int -> Int -> a -> a
getMeanSum :: Num a => [a] -> Rating a -> Int -> Int -> Int -> a
predict ratingsMatrix userIndex itemIndex | hasRating (users !! userIndex) (items !! itemIndex) ratingsMatrix = getRating (users !! userIndex) (items !! itemIndex) ratingsMatrix --Line
                                          | otherwise = predictHelper (map ((*) (-1.0)) (diffFreqMatrix (formMatrix users items ratingsMatrix))) ((formMatrix users items ratingsMatrix) !! userIndex) itemIndex 0 rowLength 0 --Mean List, user Ratings, item to be found index, current index of user list, summation accumulator, length of row
                                          where
                                            users = fromRatingsToUsers ratingsMatrix
                                            items = searchUserWithCompleteSet ratingsMatrix (fromRatingsToUsers ratingsMatrix) (fromRatingsToItems ratingsMatrix)
                                            rowLength = length items
predictHelper _ [] _ _ rowLength acc = acc / (fromIntegral rowLength - 1)
predictHelper meanList (x : xs) itemIndex currentIndex rowLength acc | x == NoRating = function acc
                                                                     | otherwise = function (acc + meanSum)
                                                                     where
                                                                      function = predictHelper meanList xs itemIndex (currentIndex + 1) rowLength
                                                                      meanSum = getMeanSum meanList x currentIndex itemIndex rowLength
getMeanSum meanList (R value) currentIndex itemIndex rowLength = value + (meanList !! ((currentIndex * rowLength) + itemIndex))

searchUserWithCompleteSet :: (Eq t, Eq a) => [(t, a, c)] -> [t] -> [a] -> [a]
userHasCompleteSet :: (Eq t2, Eq t) => [(t, t2, a)] -> t -> [t2] -> Bool
getOrderedSet :: Eq t => [(t, a, c)] -> t -> [a]
searchUserWithCompleteSet _ [] _ = error "No user has a complete ratings set"
searchUserWithCompleteSet ratings (user : tu) items = if userHasCompleteSet ratings user items then getOrderedSet ratings user else searchUserWithCompleteSet ratings tu items
userHasCompleteSet _ _ [] = True
userHasCompleteSet ratings user (item : iu) = if hasRating user item ratings then userHasCompleteSet ratings user iu else False
getOrderedSet [] _ = []
getOrderedSet ((a, b, c) : xs) user = if a == user then b : getOrderedSet xs user else getOrderedSet xs user
