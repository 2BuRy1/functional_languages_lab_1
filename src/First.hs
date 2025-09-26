module First  where
import Data.Char (digitToInt)

parseStr :: String -> [Int]
parseStr "" = []
parseStr str = map digitToInt (takeWhile (/= ' ') str)

findMaxMulTailReq :: [Int] -> Int
findMaxMulTailReq arr = innerFunc arr 0
  where
    innerFunc xs acc =
      let win = take 13 xs
      in if length win < 13
           then acc
           else
             let current = product win
                 newAcc  = max acc current
             in innerFunc (drop 1 xs) newAcc

findMaxMulReq :: [Int] -> Int
findMaxMulReq arr = innerFunc arr 0
  where
    innerFunc xs acc =
      let win = take 13 xs
      in if length win < 13
           then acc
           else
             let current = product win
             in innerFunc (drop 1 xs) (max acc current)

findMaxMulMap :: [Int] -> Int
findMaxMulMap [] = 0
findMaxMulMap arr
  | length arr < 13 = 0
  | otherwise =
      maximum (map (\i -> product (take 13 (drop i arr))) [0 .. length arr - 13])

findMaxMulFold :: [Int] -> Int
findMaxMulFold [] = 0
findMaxMulFold arr
  | length arr < 13 = 0
  | otherwise =
      foldl (\best i -> max best (product (take 13 (drop i arr)))) 0 [0 .. length arr - 13]

findMaxMulLazy :: [Int] -> Int
findMaxMulLazy [] = 0
findMaxMulLazy arr
  | length arr < 13 = 0
  | otherwise =
      maximum [ product (take 13 (drop i arr)) | i <- [0 .. length arr - 13] ]
