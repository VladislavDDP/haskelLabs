-- Лабораторна робота №4
-- Варіант 4
-- Типи i класи типiв
-- Мета роботи
-- Ознайомитись з системою типiв та класiв типiв. Набути досвiду визначення
-- нових типiв та класiв типiв i їх використання.


-- Основне завдання (#4)
-- Фiгури на площинi. Використовуються такi фiгури, як коло (центр та ра-
-- дiус), прямокутник (координати лiвої верхньої та правої нижньої точок), три-
-- кутник (координати вершин) та мiтка — label (координати лiвої нижньої точки,
-- шрифт та рядок). Доступнi шрифти — Consolas, Lucida Console та Source Code
-- Pro. Визначити функцiю для отримання прямокутника, який мiстить усi фiгури
-- iз заданого списку;

-- Додаткове завадння
-- 92. Перемiщення фiгури на вказаний вектор.


data Font = Consolas | SourceCode | Lucida deriving (Eq, Show)

data Figure = Circle Float Float Float | Rectangle Float Float Float Float | Triangle Float Float Float Float Float Float
              | TextBox  Float Float Font String deriving (Eq, Show)

getLetterSize :: Font -> Float
getLetterSize Consolas  = 8
getLetterSize SourceCode = 10
getLetterSize Lucida   = 12

getRectangles :: [Figure] -> [Figure]
getRectangles [] = []
getRectangles ((Rectangle x1 y1 x2 y2):fs) = Rectangle x1 y1 x2 y2 : getRectangles fs
getRectangles ((Circle {}):fs)          = getRectangles fs
getRectangles ((Triangle {}):fs)  = getRectangles fs
getRectangles ((TextBox {}):fs)       = getRectangles fs

move :: Figure -> Float -> Float -> Figure
move (Rectangle x1 y1 x2 y2) dx dy      = Rectangle (x1+dx) (y1+dy) (x2+dx) (y2+dy)
move (Circle x y r) dx dy               = Circle (x+dx) (y+dy) r
move (TextBox x y f s) dx dy            = TextBox (x+dx) (y+dy) f s
move (Triangle x1 y1 x2 y2 x3 y3) dx dy = Triangle (x1+dx) (y1+dy) (x2+dx) (y2+dy) (x3+dx) (y3+dy)

getBound :: Figure -> Figure
getBound (Rectangle x1 y1 x2 y2)     = Rectangle x1 y1 x2 y2
getBound (Circle x y r)              = Rectangle (x-r) (y-r) (x+r) (y+r)
getBound (TextBox x y f s)           = Rectangle x (y-getLetterSize f) (x+fromIntegral (length s) * getLetterSize f) y
getBound (Triangle x1 y1 x2 y2 x3 y3)= Rectangle (min x1 (min x2 x3)) (max y1 (max y2 y3)) (max x1 (max x2 x3)) (min y1 (min y2 y3))

getBounds :: [Figure] -> [Figure]
getBounds = map getBound

getMinX1 :: [Figure] -> Float
getMinX1 [] = 0
getMinX1 [Rectangle x1 y1 x2 y2] = x1
getMinX1 ((Rectangle x1 y1 x2 y2):xs)   = min x1 (getMinX1 xs)

getMaxX2 :: [Figure] -> Float
getMaxX2 [] = 0
getMaxX2 [Rectangle x1 y1 x2 y2] = x2
getMaxX2 ((Rectangle x1 y1 x2 y2):xs)   = max x2 (getMaxX2 xs)

getMinY1 :: [Figure] -> Float
getMinY1 [] = 0
getMinY1 [Rectangle x1 y1 x2 y2] = y1
getMinY1 ((Rectangle x1 y1 x2 y2):xs)   = min y1 (getMinY1 xs)

getMaxY2 :: [Figure] -> Float
getMaxY2 [] = 0
getMaxY2 [Rectangle x1 y1 x2 y2] = y2
getMaxY2 ((Rectangle x1 y1 x2 y2):xs)   = max y2 (getMaxY2 xs)

getRectangle :: [Figure] -> Figure
getRectangle x = Rectangle (getMinX1 (getBounds x))  (getMinY1 (getBounds x)) (getMaxX2 (getBounds x)) (getMaxY2 (getBounds x))


-- Тестування 
-- Основне завдання 
-- Вхідні дані: getRectangle [Circle 1 1 5, Rectangle 0 0 5 5, Triangle 0 0 5 5 5 0]
-- Результат: Rectangle (-4.0) (-4.0) 6.0 6.0

-- Додаткове завдання
-- Вхідні дані: move (Rectangle 0 0 5 5) 2 2
-- Результат: Rectangle 2.0 2.0 7.0 7.0
  

-- Висновок 
-- На даній лабораторній роботі ми ознайомились з системою типiв та класiв типiв, набути досвiду визначення
-- нових типiв та класiв типiв i їх використання. В якості завдань ми обрали основне - побудувати фігуру (прямокутник), 
-- що буде вміщувати в собі інші фігури та додаткове - переміщення фігури на заданий ветктор.