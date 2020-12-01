import Prelude 

main = do
  print . show . g . map (read :: String -> Int) . words =<< readFile "input1.txt"

g :: [Int] -> Int
g (x:xs) = case f (2020 - x) xs of
  Nothing -> g xs
  Just y  -> y * x

f :: Int -> [Int] -> Maybe Int
f n (x:xs) = if (n - x) `elem` xs then Just (x * (n - x)) else f n xs
f _ [] = Nothing
