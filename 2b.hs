import Text.Parsec

main = do
  content <- readFile "input2.txt"
  print . f . extract . map (parse p "") . lines $ content

f xs = calc True $ map check xs

check (low, high, c, s) = let pos1  = s !! (low - 1)
                              pos2  = s !! (high - 1)
                          in (c == pos1) /= (c == pos2)

calc c s = length $ filter (==c) s
 
extract (Right x:xs) = x:extract xs
extract (_:xs) = extract xs
extract [] = []

p :: Parsec String() (Int, Int, Char, String)
p = do
  low <- many1 digit
  char '-'
  high <- many1 digit
  char ' '
  c <- letter
  string ": "
  s <- many1 letter
  return $ (read low, read high, c, s)
