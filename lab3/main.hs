-- Лабораторна робота 3
-- Виконав студент групи КН-31 Кучерук Владислав

-- 1.4 Послiдовнiсть тотожних елементiв списку замiнити одним елементом,
-- напр.: [1,1,1,5,5,3, 1,1,222,222,222,222] ⇒ [1,5,3,1,222].

-- а) без застосування 
headCustom :: [a] -> a
headCustom (x:xs) = x

uniquePure :: Eq a => [a] -> [a]
uniquePure [] = []
uniquePure [x] = [x]
uniquePure (x:xs) = if x == headCustom xs
                    then uniquePure xs
                    else x : uniquePure xs


-- б) з застосуванням вбудованих функцiй.
uniqueBuildIn :: Eq a => [a] -> [a]
uniqueBuildIn [] = []
uniqueBuildIn [x] = [x]
uniqueBuildIn (x:xs)
            | x == head xs = uniqueBuildIn xs
            | otherwise = x : uniqueBuildIn xs


removeDuplicates2 :: Eq a => [a] -> [a]
removeDuplicates2 = foldl (\seen x -> if x `elem` seen
                                      then seen
                                      else seen ++ [x]) []
-- Testing 
-- case 1: [1,1,1,1,1,1,1,2,2,2,2,55,888,888,888]
-- answer: [1,2,55,888]

-- case 2: [99, 99, 88, 88, 77]
-- answer: [1,2,55,888]

-- case 3: [1,1,1,5,5,3, 1,1,222,222,222,222]
-- answer: [1,5,3,1,222]


-- 2.4 Визначити, чи два числа взаємно простi.

-- а) без застосування 
euclid :: Integer -> Integer -> Integer
euclid n m
  | n == m = n
  | n < m = euclid n (m-n)
  | otherwise = euclid (n-m) m

matualluSimpleNumbersPure :: Integer -> Integer -> Bool
matualluSimpleNumbersPure x y = euclid x y == 1


-- б) з застосуванням вбудованих функцiй.
matualluSimpleNumbersBuildIn :: Integer -> Integer -> Bool
matualluSimpleNumbersBuildIn x y = gcd x y == 1


-- Testing 
-- case 1: 5 15
-- answer: False

-- case 2: 13 16
-- answer: True

-- case 3: 100 1
-- answer: True


-- Висновок
-- На даній лабораторній роботі ми навчилися використовувати функції вищих порядків в мови Haskell.
-- В своїх завданнях ми застосували функції, що вбудовані в мову та викликали інші, для реалізації
-- поставлених завдань.

