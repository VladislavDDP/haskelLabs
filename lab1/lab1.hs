-- Лабораторна робота 1
-- Виконав студент групи КН-31 Кучерук Владислав

-- Перша частина

-- [([Integer],[String])]
a = [([1, 2, 3], ["Hello", "World"])]

-- [[(Integer,[Bool])]]
b = [[(1, [True, False, 1 == 1, 2 > 3])]]

-- [([Bool],[String],[Double])]
c = [([1 < 2, False], ["Love JS"], [1.23, 5.234])]

-- ((String,Integer), Char, [Double])
d = (("Santa", 1), 'g', [13.23, 42.234])

-- ([Char],[Double],[(Bool,Integer)])
e = (['b'], [3.222, 4.32234],[(True, 1)])

-- [(Integer, (Char,[Bool])]
f = [(1, ('a',[True, False]))]



-- Друга частина 

-- Функцiя приймає три числа i перевiряє, чи значення першого з них
-- знаходиться мiж значеннями двох iнших.

checkIfBetween :: (Integer, Integer, Integer) -> Bool
checkIfBetween (x, y, z) =  y <= x && x <=  z

checkIfBetweenOrder :: Integer -> Integer -> Integer -> Bool
checkIfBetweenOrder x y z =  y <= x && x <=  z


-- Функцiя за довжиною трьох вiдрiзкiв визначає, чи можна на них побудувати прямокутний трикутник.

checkIfCanBuildTriangle :: (Integer, Integer, Integer) -> Bool
checkIfCanBuildTriangle (x, y, z) = x + y > z && z + y > x && x + z > y

checkIfCanBuildTriangleOrder :: Integer -> Integer -> Integer -> Bool
checkIfCanBuildTriangleOrder x y z = x + y > z && z + y > x && x + z > y


-- Функцiя приймає двi логiчнi величини (Bool) i повертає їх у формi
-- впорядкованої за спаданням двiйки (кортежа).

sortLogic :: (Bool, Bool) -> (Bool, Bool)
sortLogic (x, y) = if x > y then (x, y) else (y, x)

sortLogicOrder :: Bool -> Bool -> (Bool, Bool)
sortLogicOrder x y = if x > y then (x, y) else (y, x)


-- Функцiя приймає два рядки (String) i перевiряє, чи вони лексикографiчно впорядкованi.
checkIfLeksicalSorted :: (String, String) -> Ordering
checkIfLeksicalSorted (x, y) = compare x  y

checkIfLeksicalSortedOrder :: String -> String -> Ordering
checkIfLeksicalSortedOrder = compare


-- Висновок
-- На даній лабораторній роботі ми ознайомитись з основними типами даних мови Haskell.
-- Ознайомилися зi структурою та функцiями Glasgow Haskell Compiller. Набули навичок роботи з iнтерпретатором
-- ghci та визначили найпростiші функцiї такі як: перевірка на лексикографічний порядок, перевірка на інтервал
-- і т. п.