-- Лабораторна робота 5.1
-- Виконав студент групи КН-31 Кучерук Владислав

import System.Directory.Internal.Prelude (getArgs)

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = foldl (\seen x -> if x `elem` seen
                                      then seen
                                      else seen ++ [x]) []

f :: [Int] -> [String]
f = map show

main :: IO ()
main = do 
    --read from console
    input_array <- getLine 
    let parsed_array = map read $ words input_array :: [Int]

    --write in console
    print ( removeDuplicates parsed_array )

    --read from file
    contents <- readFile "input.txt" 
    let parsed_from_file = map read $ words contents :: [Int] 
    print ( removeDuplicates parsed_from_file )

    --write into file
    let output = removeDuplicates parsed_from_file
    writeFile "output.txt" (concat (f output))

-- Тестування
-- У файлі для введення: 1 1 1 1 1 1 1 2 2 2 2 55 888 888 888
-- Результат у файлі для виведення: 1 2 55 888

-- Введення з клавіатури: 1 1 1 2 2 3 4 5 6 7 8 8
-- Результат у консолі: 1 2 3 4 5 6 7 8

-- Висновок
-- На даній лабораторній роботі ми навчилися працювати з потоками введення та виведення у консолі та файлах
-- Для цього ми використали вбудовані функції, що представлені у Haskell. Додатково ми описали функції для
-- конвертування типів даних у строкові типи, чи навпаки для коректної роботи вбудованих методів.