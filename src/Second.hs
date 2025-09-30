module Second where

isAbundant :: Int -> Bool
isAbundant n = sum (divs n) > n

abundants :: Int -> [Int]
abundants limit = [x | x <- [1 .. limit], isAbundant x]

isSumOfTwoAbundants limit n =
  let xs = takeWhile (<= n) (abundants limit)
   in any (\a -> (n - a) `elem` xs) (takeWhile (<= n `div` 2) xs)

------------------------- Tail Req -----------------
divs :: Int -> [Int]
divs n = innerFunc 1 []
  where
    innerFunc d acc
      | d == n = reverse acc
      | n `mod` d == 0 = innerFunc (d + 1) (d : acc)
      | otherwise = innerFunc (d + 1) acc

countTailReq :: Int -> Int
countTailReq limit = loop 0 1
  where
    loop acc n
      | n > limit = acc
      | isSumOfTwoAbundants limit n = loop acc (n + 1)
      | otherwise = loop (acc + n) (n + 1)

-------------------------------------------------------

-----------------------------Lin Req----------------------
linDivs :: Int -> [Int]
linDivs n = innerFunc 1
  where
    innerFunc d
      | d == n = []
      | n `mod` d == 0 = d : innerFunc (d + 1)
      | otherwise = innerFunc (d + 1)

isAbundantLin :: Int -> Bool
isAbundantLin n = sum (linDivs n) > n

abundantsLin :: Int -> [Int]
abundantsLin limit = [x | x <- [1 .. limit], isAbundantLin x]

isSumOfTwoAbundantsLin limit n =
  let xs = takeWhile (<= n) (abundantsLin limit)
   in any (\a -> (n - a) `elem` xs) (takeWhile (<= n `div` 2) xs)

countLinReq :: Int -> Int
countLinReq limit = loop 1
  where
    loop n
      | n > limit = 0
      | isSumOfTwoAbundantsLin limit n = loop (n + 1)
      | otherwise = n + loop (n + 1)

-----------------------------------------------------------
-------------------------------map-------------------------
countMap :: Int -> Int
countMap limit =
  sum [n | n <- [1 .. limit], not (isSumOfTwoAbundants limit n)]

------------------------------foldl------------------------
sumDivsFold n =
  foldl (\acc d -> if n `mod` d == 0 then acc + d else acc) 0 [1 .. n - 1]

isAbundantFold :: Int -> Bool
isAbundantFold n = sumDivsFold n > n

abundantsFold :: Int -> [Int]
abundantsFold limit = [x | x <- [1 .. limit], isAbundantFold x]

isSumOfTwoAbundantsFold :: Int -> Int -> Bool
isSumOfTwoAbundantsFold limit n =
  let xs = takeWhile (<= n) (abundantsFold limit)
      left = takeWhile (<= n `div` 2) xs
   in any (\a -> (n - a) `elem` xs) left

countFold :: Int -> Int
countFold limit =
  foldl
    (\acc n -> if isSumOfTwoAbundantsFold limit n then acc else acc + n)
    0
    [1 .. limit]

---------------------------------------------------------
-----------------------------lazy------------------------
abundantsLazy :: [Int]
abundantsLazy = filter isAbundant [1 ..]

isSumOfTwoAbundantsLazy :: Int -> Bool
isSumOfTwoAbundantsLazy n =
  let xs = takeWhile (<= n) abundantsLazy
      left = takeWhile (<= n `div` 2) xs
   in any (\a -> (n - a) `elem` xs) left

countLazy :: Int -> Int
countLazy limit = sum [n | n <- [1 .. limit], not (isSumOfTwoAbundantsLazy n)]
