import Data.Char(ord,chr)

caesar :: [Char] -> Int -> [Char]
caesar s n = map chr $ map (\x -> rotChar x n) $ map ord s

rotChar c n
 | n > 26     = rotChar c $ mod n 26
 | a > 122    = a - 26 -- lowercase letters
 | a > 96 + n = a
 | a > 90     = a - 26 -- capital letters
 | a > 64 + n = a
 | otherwise = c       -- other symbols
 where a = c + n


decode :: [Char] -> [Char]
decode s = decode' s 1 0
decode' s n h
 | n > 26 = caesar s h
 | score s n > score s h = decode' s (n + 1) n 
 | otherwise = decode' s (n + 1) h

-- score each possible cipher based on letter frequency
score s n = sum $ map (\x -> scoreGrid!!(toOrd x)) $ caesar s n
scoreGrid = [0,3,-1,1,1,4,0,0,2,2,-5,-2,1,0,2,3,0,-6,2,2,3,1,-1,0,-5,0,-7]
toOrd c
 | a > 96 && a <= 122 = a - 96 -- lowercase letters
 | a > 64 && a <= 90  = a - 64 -- capital letters
 | otherwise = 0               -- other symbols
 where a = ord c
