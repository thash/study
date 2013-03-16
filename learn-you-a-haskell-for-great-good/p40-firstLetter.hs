firstLetter :: String -> String
firstLetter "" = "Empty string. whoops!"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- *Main> firstLetter "hey"
-- "The first letter of hey is h"
