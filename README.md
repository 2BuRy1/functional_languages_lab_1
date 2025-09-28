# Федоров Евгений Коснтантинович
- Первая задача [Первая задача](https://projecteuler.net/problem=8) 
- Вторая задача [Вторая задача](https://projecteuler.net/problem=23)

# Реализация на язке Haskell 

## [Первая задача](https://projecteuler.net/problem=8)
Реализация через хвостовую рекурсию: 
```haskell
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
```
Реализация через обычную рекурсию: 
```haskell
findMaxMulReq :: [Int] -> Int
findMaxMulReq arr = innerFunc arr
    where
        innerFunc xs =
            let win = take 13 xs
            in if length win < 13
                then 0
                else
                    let current = product win
                        rest    = innerFunc (drop 1 xs)
                    in max current rest
```

Реализация через свертку (foldl): 
```haskell
findMaxMulFold :: [Int] -> Int
findMaxMulFold [] = 0
findMaxMulFold arr
  | length arr < 13 = 0
  | otherwise =
      foldl (\best i -> max best (product (take 13 (drop i arr)))) 0 [0 .. length arr - 13]
```
Реализация через map
```haskell
findMaxMulMap :: [Int] -> Int
findMaxMulMap [] = 0
findMaxMulMap arr
  | length arr < 13 = 0
  | otherwise =
      maximum (map (\i -> product (take 13 (drop i arr))) [0 .. length arr - 13])
```

Реализация через ленивые итераторы и бесконечные списки: 
```haskell
digits :: Int -> [Int]
digits x = map (read . (:[])) (filter (/= '-') (show x))

findMaxMulLazy :: Int -> Int
findMaxMulLazy n =
  let arr = digits (head (take 1 [n..])) 
  in if length arr < 13
       then 0
       else maximum [ product (take 13 (drop i arr))
                    | i <- [0 .. length arr - 13] ]
```

Реализация на языке Java: 
```haskell
import java.util.Scanner;
import java.util.ArrayList;
import java.util.List;

public class First{


private static final int k = 13;
public static void main(String... args) {

        
        String s = new Scanner(System.in).nextLine();


        List<Integer> digitsList = new ArrayList<>();
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (Character.isDigit(c)) {
                digitsList.add(c - '0');
            }
        }
        if (digitsList.size() < k || k <= 0) return;

        int n = digitsList.size();
        int maxProd = 0;

        for (int i = 0; i + k <= n; i++) {
            int prod = 1;
            for (int j = 0; j < k; j++) {
                prod *= digitsList.get(i + j);
            }
            if (prod > maxProd) maxProd = prod;
        }

        System.out.println(maxProd);
    }
}
```


Реализация алгоритма на языке Haskell выглядит намного приятнее во всех ее проявлениях, меньше кода - меньше вникать => легче понять. 
Фунции имеют "понятные" названия, что помогают понять, что происходит в коде даже тому человеку, который никогда не писал на этом языке.


## [Вторая задача](https://projecteuler.net/problem=23)

Реализация через хвостовую рекурсию:
```haskell
countTailReq :: Int -> Int
countTailReq limit = loop 0 1
  where
    loop acc n
      | n > limit                 = acc
      | isSumOfTwoAbundants limit n     = loop acc       (n + 1)
      | otherwise                 = loop (acc + n) (n + 1)
```
Реализация через обычную рекурсию:
```haskell
countLinReq :: Int -> Int
countLinReq limit = loop 1
    where
        loop n
            | n > limit                 = 0
            | isSumOfTwoAbundantsLin limit n     = loop  (n + 1)
            | otherwise                 = n + loop (n + 1)
```

Реализация через свертку (foldl):
```haskell
countFold :: Int -> Int
countFold limit =
  foldl
    (\acc n -> if isSumOfTwoAbundantsFold limit n then acc else acc + n)
    0
    [1..limit]
```
Реализация через map
```haskell
countMap ::Int -> Int
countMap limit =
  sum [ n | n <- [1..limit], not (isSumOfTwoAbundants limit n) ]
```

Реализация через ленивые итераторы и бесконечные списки:
```haskell
countLazy :: Int -> Int
countLazy limit = sum [ n | n <- [1..limit], not (isSumOfTwoAbundantsLazy n) ]
```

Реализация на языке Java:
```haskell
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;
public class Second{
public static void main(String...args){
    int limit = new Scanner(System.in).nextInt();
    if (limit <= 0) return ;

        List<Integer> abundants = new ArrayList<>();
        for (int n = 1; n <= limit; n++) {
            if (isAbundant(n)) abundants.add(n);
        }

        boolean[] can = new boolean[limit + 1];
        for (int i = 0; i < abundants.size(); i++) {
            int a = abundants.get(i);
            for (int j = i; j < abundants.size(); j++) { 
                int b = abundants.get(j);
                int sum = a + b;
                if (sum > limit) break;
                can[sum] = true;
            }
        }

        long total = 0;
        for (int n = 1; n <= limit; n++) {
            if (!can[n]) total += n;
        }
        System.out.println(total);
}


 public static boolean isAbundant(int n) {
        return sumProperDivisors(n) > n;
    }

    public static int sumProperDivisors(int n) {
        if (n <= 1) return 0;    
        int sum = 1;           
        int r = (int) Math.sqrt(n);

        for (int d = 2; d <= r; d++) {
            if (n % d == 0) {
                int q = n / d;
                sum += d;
                if (q != d) sum += q; 
            }
        }
        return sum;
    }
}
```

Аналогично и для этой задачи. Императивный подход к решению задачи плодит огроменное количество строк кода, что сильно снижает его восприятие, Haskell проявляет себя с отличной стороны.

# Итог 
Haskell синтаксически интересный язык, а его работа вызывает удивления, я познакомился с концепцией гуардов, ленивых вычислений. Один из самых удобных языков, с которым мне приходилось иметь дело.