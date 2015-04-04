module Main where

import Control.Applicative
import qualified Data.Foldable as F

import Graphics.Gloss.Data.Vector (rotateV)
import Graphics.Gloss.Interface.Pure.Game

-- | Для генерации случайных значений используем целые числа.
data Gen = Gen Int deriving (Show)

-- | Случайное значение типа a.
data Rand a = Rand (Gen -> (a, Gen))

-- | Получить случайное значение, подав на вход зерно генератора случайных чисел.
getRand :: Rand a -> Gen -> (a, Gen)
getRand (Rand f) = f

instance Functor Rand where
  fmap f (Rand g) = Rand (\n -> let (x, n') = g n in (f x, n'))

instance Applicative Rand where
  pure x = Rand (\n -> (x, n))
  Rand f <*> Rand g = Rand (\n -> let (x, n') = f n
                                      (y, n'') = g n'
                                  in (x y, n''))

-- | Сгенерировать случайное число от 0 до maxRand.
--
-- Для генерации используется линейный конгруэнтный метод:
--   X(n+1) = (a * Xn + c) mod m
rand :: Rand Int
rand = Rand (\g -> let g' = next g in (res g', g'))
  where
    res  (Gen n) = n `mod` maxRand
    next (Gen n) = Gen ((a * n + c) `mod` m)
    (a, c, m) = (1366, 150889, 714025)

-- | Максимальное случайное число, сгенерированное при помощи rand.
maxRand :: Num a => a
maxRand = 2^16

-- | Геометрическая фигура с координатами центра, списком вершин и цветом.
data Figure = Figure Point Path Color

-- | Построить правильный многоугольник с радиусом 1.
equilateral :: Int -> Path
equilateral n = take n (iterate (rotateV a) (0, 1))
  where
    a = 2 * pi / fromIntegral n

-- | Правильный многоугольник со случайным количеством вершин от 3 до 10.
randPolygon :: Rand Path
randPolygon = pure (\n -> equilateral (3 + n `mod` 8)) <*> rand

-- | Случайный цвет.
randColor :: Rand Color
randColor = pure makeColor <*> x <*> x <*> x <*> pure 1
  where
    -- случайное значение от 0 до 1
    x = pure (\n -> fromIntegral n / maxRand) <*> rand

-- | Случайная фигура с заданным центром.
randFigure :: Point -> Rand Figure
randFigure pos = pure (Figure pos) <*> randPolygon <*> randColor

-- | Мир — множество фигур и генератор случайных чисел.
data World = World Gen [Figure]

-- | Начальный мир без фигур с заданным генератором случайных чисел.
initialWorld :: Gen -> World
initialWorld g = World g []

-- | Отрисовка мира.
drawWorld :: World -> Picture
drawWorld (World _ fs) = F.foldMap drawFigure (reverse fs)

-- | Отрисовка фигуры.
drawFigure :: Figure -> Picture
drawFigure (Figure (x, y) path c) = translate x y (scale 50 50 (color c (polygon path)))

-- | Добавить случайную фигуру в указанной точке.
addFigure :: Point -> World -> World
addFigure pos (World g fs) = World g' (f:fs)
  where
    (f, g') = getRand (randFigure pos) g

-- | Обработка событий.
handleWorld :: Event -> World -> World
handleWorld (EventKey (MouseButton LeftButton) Down _ (x, y)) = addFigure (x, y)
handleWorld _ = id

-- | Обновление мира (ничего не происходит).
updateWorld :: Float -> World -> World
updateWorld _ = id

main :: IO ()
main = do
  play display bgColor fps (initialWorld seed) drawWorld handleWorld updateWorld
  where
    windowSize   = (640, 480)
    windowOffset = (200, 200)
    display = InWindow "Random figures" windowSize windowOffset
    bgColor = white
    fps = 60
    seed = Gen 31337

