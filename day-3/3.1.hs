import Data.Char(digitToInt)

main :: IO ()
<<<<<<< HEAD
main = show . (\x -> (2**12 - 1 - x) * x)  . sum . (map uncurry (*)) . zip powers . map mc . map (map digitToInt) . lines <$> getContents >>= putStrLn
=======
main = show . (\x -> (2**12 - 1 - x) * x)  . sum . map (uncurry (*)) . zip powers . map mc . map (map digitToInt) . lines <$> getContents >>= putStrLn
>>>>>>> e3d360a (Very slightly polish code)
    where powers = [2^(11-i) | i <- [0..]]
          mc xs = mc' 0 0 xs
          mc' x y [] = if x > y then 0 else 1
          mc' x y (d:ds) = if d == 0 then mc' (x+1) y ds else mc' x (y + 1) ds

