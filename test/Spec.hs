import Test.HUnit
import First hiding (main)
import Second


t_findMaxMul_edges :: Test
t_findMaxMul_edges = TestLabel "findMaxMul* edge cases" $ TestList
  [ TestCase (assertEqual "TailReq len<13" 0 (findMaxMulTailReq [1..12]))
  , TestCase (assertEqual "Req    len<13"  0 (findMaxMulReq     [1..12]))
  , TestCase (assertEqual "Map    len<13"  0 (findMaxMulMap     [1..12]))
  , TestCase (assertEqual "Fold   len<13"  0 (findMaxMulFold    [1..12]))
  , TestCase (assertEqual "Lazy   len<13"  0 (findMaxMulLazy    123456789012))

  , TestCase (let arr = replicate 13 1
              in assertEqual "TailReq 13x1" 1 (findMaxMulTailReq arr))
  , TestCase (let arr = replicate 13 1
              in assertEqual "Req    13x1"  1 (findMaxMulReq     arr))
  , TestCase (let arr = replicate 13 1
              in assertEqual "Map    13x1"  1 (findMaxMulMap     arr))
  , TestCase (let arr = replicate 13 1
              in assertEqual "Fold   13x1"  1 (findMaxMulFold    arr))
  , TestCase (let arr = replicate 13 1
              in assertEqual "Lazy   13x1"  1 (findMaxMulLazy    1111111111111))

  , TestCase (let arr = [1,1,1,1,1,0,1,1,1,1,1,1,1]
              in assertEqual "TailReq 13 with zero" 0 (findMaxMulTailReq arr))
  , TestCase (let arr = [1,1,1,1,1,0,1,1,1,1,1,1,1]
              in assertEqual "Req    13 with zero"  0 (findMaxMulReq     arr))
  , TestCase (let arr = [1,1,1,1,1,0,1,1,1,1,1,1,1]
              in assertEqual "Map    13 with zero"  0 (findMaxMulMap     arr))
  , TestCase (let arr = [1,1,1,1,1,0,1,1,1,1,1,1,1]
              in assertEqual "Fold   13 with zero"  0 (findMaxMulFold    arr))
  , TestCase (let arr = [1,1,1,1,1,0,1,1,1,1,1,1,1]
              in assertEqual "Lazy   13 with zero"  0 (findMaxMulLazy    1111101111111))
  ]

t_findMaxMul_clearMax :: Test
t_findMaxMul_clearMax = TestLabel "findMaxMul* clear maximum windows" $ TestList
  [ (let arr = replicate 13 2 ++ [1,2,3]; expected = 8192
     in TestCase (assertEqual "TailReq leading 2^13" expected (findMaxMulTailReq arr)))
  , (let arr = replicate 13 2 ++ [1,2,3]; expected = 8192
     in TestCase (assertEqual "Req    leading 2^13"  expected (findMaxMulReq     arr)))
  , (let arr = replicate 13 2 ++ [1,2,3]; expected = 8192
     in TestCase (assertEqual "Map    leading 2^13"  expected (findMaxMulMap     arr)))
  , (let arr = replicate 13 2 ++ [1,2,3]; expected = 8192
     in TestCase (assertEqual "Fold   leading 2^13"  expected (findMaxMulFold    arr)))
  , (let arr = replicate 13 2 ++ [1,2,3]; expected = 8192
     in TestCase (assertEqual "Lazy   leading 2^13"  expected (findMaxMulLazy    2222222222222123)))

  , (let arr = replicate 10 2 ++ [0] ++ replicate 13 5 ++ [7,7,7]; expected = 3349609375
     in TestCase (assertEqual "TailReq after zero 5^13" expected (findMaxMulTailReq arr)))
  , (let arr = replicate 10 2 ++ [0] ++ replicate 13 5 ++ [7,7,7]; expected = 3349609375
     in TestCase (assertEqual "Req    after zero 5^13"  expected (findMaxMulReq     arr)))
  , (let arr = replicate 10 2 ++ [0] ++ replicate 13 5 ++ [7,7,7]; expected = 3349609375
     in TestCase (assertEqual "Map    after zero 5^13"  expected (findMaxMulMap     arr)))
  , (let arr = replicate 10 2 ++ [0] ++ replicate 13 5 ++ [7,7,7]; expected =3349609375
     in TestCase (assertEqual "Fold   after zero 5^13"  expected (findMaxMulFold    arr)))
  , (let arr = replicate 10 2 ++ [0] ++ replicate 13 5 ++ [7,7,7]; expected = 3349609375
     in TestCase (assertEqual "Lazy   after zero 5^13"  expected (findMaxMulLazy    220555555555555777)))

  , (let arr = replicate 13 3; expected = 1594323
     in TestCase (assertEqual "TailReq 13x3" expected (findMaxMulTailReq arr)))
  , (let arr = replicate 13 3; expected = 1594323
     in TestCase (assertEqual "Req    13x3"  expected (findMaxMulReq     arr)))
  , (let arr = replicate 13 3; expected = 1594323
     in TestCase (assertEqual "Map    13x3"  expected (findMaxMulMap     arr)))
  , (let arr = replicate 13 3; expected = 1594323
     in TestCase (assertEqual "Fold   13x3"  expected (findMaxMulFold    arr)))
  , (let arr = replicate 13 3; expected = 1594323
     in TestCase (assertEqual "Lazy   13x3"  expected (findMaxMulLazy    3333333333333)))
  ]


t_counts_equivalence :: Test
t_counts_equivalence = TestLabel "counts equivalence (limit=100)" $ TestList
  [ TestCase (assertBool "Tail vs Lin"  ((countTailReq 100) == (countLinReq 100)))
      , TestCase (assertBool "Tail vs Map"  ((countTailReq 100) == (countMap 100)))
  , TestCase (assertBool "Tail vs Fold" ((countTailReq 100) == (countFold 100)))
  , TestCase (assertBool "Tail vs Lazy" ((countTailReq 100) == (countLazy 100)))
  ]

t_counts_expected_100 :: Test
t_counts_expected_100 = TestLabel "expected totals for limit=100" $ TestList
  [ TestCase (assertEqual "countTailReq" 2766 (countTailReq 100))
  , TestCase (assertEqual "countLinReq"  2766 (countLinReq 100))
  , TestCase (assertEqual "countMap"     2766 (countMap 100))
  , TestCase (assertEqual "countFold"    2766 (countFold 100))
  , TestCase (assertEqual "countLazy"    2766 (countLazy 100))
  ]

t_counts_sanity :: Test
t_counts_sanity = TestLabel "counts sanity (limit=100)" $ TestList
  [ TestCase (assertBool "Tail > 0" ((countTailReq 100) > 0))
  , TestCase (assertBool "Lin  > 0" ((countLinReq 100)  > 0))
  , TestCase (assertBool "Map  > 0" ((countMap 100)    > 0))
  , TestCase (assertBool "Fold > 0" ((countFold 100)   > 0))
  , TestCase (assertBool "Lazy > 0" ((countLazy 100)   > 0))
  ]

t_counts_negative :: Test
t_counts_negative = TestLabel "counts with negative limit" $ TestList
   [ TestCase (assertEqual "TailReq -10" 0 (countTailReq (-10)))
   , TestCase (assertEqual "LinReq  -10" 0 (countLinReq  (-10)))
   , TestCase (assertEqual "Map     -10" 0 (countMap     (-10)))
   , TestCase (assertEqual "Fold    -10" 0 (countFold    (-10)))
   , TestCase (assertEqual "Lazy    -10" 0 (countLazy    (-10)))
   ]

tests :: Test
tests = TestList
      [ t_findMaxMul_edges
      , t_findMaxMul_clearMax
      , t_counts_equivalence
      , t_counts_expected_100
      , t_counts_sanity
      , t_counts_negative
      ]

main :: IO Counts
main = runTestTT tests
