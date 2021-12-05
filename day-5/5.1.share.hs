import Data.List
import Control.Monad (join)
import Control.Arrow ((***))

mapTuple = join (***)


data LineSegment = LineSegment {
  a :: Rational,
  b :: Rational,
  c :: Rational,
  x1 :: Rational,
  x2 :: Rational,
  y1 :: Rational,
  y2 :: Rational
} -- where ax + by + c == 0

instance Show LineSegment where
  show LineSegment {
    a = a,
    b = b,
    c = c
  } = "LineSegment { a = " ++ (show a) ++ ", b = " ++ (show b) ++ ", c = " ++ (show c) ++" }"

data SquareConvexHull = SquareConvexHull {
  sx1 :: Rational,
  sx2 :: Rational,
  sy1 :: Rational,
  sy2 :: Rational
}

main :: IO ()
main = do
  input <- getContents
  let segments = parse input
  let horizontalAndVerticalSegments = filter isHorizontalOrVertical segments
  -- putStrLn $ intercalate "\n" $ map show segments
  -- putStrLn ""
  -- putStrLn $ intercalate "\n" $ map show horizontalAndVerticalSegments
  -- putStrLn $ show $ length $ horizontalAndVerticalSegments
  let grid = (convexHullToGrid . calculateConvexHull) horizontalAndVerticalSegments
  let dangerousPoints = filter (dangerous horizontalAndVerticalSegments) grid
  let answer = length dangerousPoints
  putStrLn $ show answer

(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) f g x = (f x) || (g x)  


isHorizontalOrVertical :: LineSegment -> Bool
isHorizontalOrVertical = isHorizontal ||| isVertical

isHorizontal :: LineSegment -> Bool
isHorizontal LineSegment {y1 = y1, y2 = y2} = y1 == y2

isVertical :: LineSegment -> Bool
isVertical LineSegment {x1 = x1, x2 = x2} = x1 == x2

dangerous :: [LineSegment] -> (Rational, Rational) -> Bool
dangerous segments point = ((>=2) . length . filter (contains point)) segments

contains :: (Rational, Rational) -> LineSegment -> Bool
contains point@(x, y) segment@LineSegment{a = a, b = b, c = c} = (a*x + b*y + c == 0)  && inBounds point segment

inBounds :: (Rational, Rational) -> LineSegment -> Bool
inBounds point@(x, y) segment@LineSegment{x1=x1, x2=x2, y1=y1, y2=y2} = (x1 <= x && x <= x2) && (y1 <= y && y <= y2) 

convexHullToGrid :: SquareConvexHull -> [(Rational, Rational)]
convexHullToGrid SquareConvexHull {
  sx1 = sx1,
  sx2 = sx2,
  sy1 = sy1,
  sy2 = sy2
} = prod [sx1..sx2] [sy1..sy2]

prod xs ys = [(x,y) | x <- xs, y <- ys]

updateConvexHull :: SquareConvexHull -> LineSegment -> SquareConvexHull
updateConvexHull convexHull@SquareConvexHull {
  sx1 = sx1,
  sx2 = sx2,
  sy1 = sy1,
  sy2 = sy2
} LineSegment {
  x1 = x1,
  x2 = x2,
  y1 = y1,
  y2 = y2} = convexHull {
    sx1 = min sx1 x1,
    sx2 = max sx2 x2,
    sy1 = min sy1 y1,
    sy2 = max sy2 y2
}

fromLineSegment :: LineSegment -> SquareConvexHull
fromLineSegment LineSegment{x1=x1, x2=x2, y1=y1, y2=y2} = SquareConvexHull {
  sx1 = x1,
  sx2 = x2,
  sy1 = y1,
  sy2 = y2
}

calculateConvexHull :: [LineSegment] -> SquareConvexHull
calculateConvexHull (x:xs) = foldl updateConvexHull (fromLineSegment x) xs

parse :: String -> [LineSegment]
parse = map readSegment . lines

readSegment :: String -> LineSegment
readSegment = uncurry createSegment . mapTuple parseTuple . firstAndLast . words

parseTuple :: String -> (Int, Int)
parseTuple = read . (flip (++)) ")" . (:) '('

firstAndLast :: [a] -> (a, a)
firstAndLast list = (head list, last list)

{-
 - If a line passes through (x1, y1) and (y1, y2)
 - We have to solve
 - a * x1 + b * y1 = -1
 - a * x2 + b * y2 = -1
 - Which is the same as solving
 - [x1 y1; x2 y2] [a; b] = [-1, -1]
 - The solution is [a; b] = [x1 y1 -1; x2 y2 -1]^-1 [-1; -1]
 -
 - For the case where one of the points is (0, 0)
 - We solve
 - x1 + b * y1 = 0
 - a = 1
 - c = 0
 - b = - x1/y1
-} 

createSegment :: (Int, Int) -> (Int, Int) -> LineSegment
createSegment (x1, y1) (x2, y2) = createSegment' (fi x1, fi y1) (fi x2, fi y2)
  where fi = fromIntegral

createSegment' :: (Rational, Rational) -> (Rational, Rational) -> LineSegment
createSegment' (0, 0) (x, y) = LineSegment{a = 1, b = -x/y, c = 0, x1 = 0, x2 = x, y1 = 0, y2 = y}
createSegment' point@(x, y) zero@(0, 0) = createSegment' zero point -- LineSegment{a = 1, b = -x/y, c = 0, x1 = 0, x2 = x, y1 = 0, y2 = y} 
createSegment' (x1, y1) (x2, y2) = LineSegment {
  a = y1 / (x1 * y2 - x2 * y1) - y2 / (x1 * y2 - x2 * y1),
  b = x2/(x1 * y2 - x2 * y1) - x1 / (x1 * y2 - x2 * y1),
  c = 1,
  x1 = min x1 x2,
  x2 = max x1 x2,
  y1 = min y1 y2,
  y2 = max y1 y2
}
