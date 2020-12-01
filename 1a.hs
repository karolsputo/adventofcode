main = do
  print . show . f . map (read :: String -> Int) . words =<< readFile "input1.txt"

f :: [Int] -> Int
f (x:xs) = if (2020 - x) `elem` xs then x * (2020 - x) else f xs
