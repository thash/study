lend amount balance = let reserve = 100
                          newBalance = balance - amount
                      in if balance < reserve
                         then Nothing
                         else Just newBalance

-- 3.9.3 局所変数と大域変数
pluralize :: String -> [Int] -> [String]
pluralize word counts = map plural counts -- 本体が明らかだ
    where plural 0 = "no " ++ word ++ "s"
          plural 1 = "one " ++ word
          plural n = show n ++ " " ++ word ++ "s"

-- *Main> pluralise "apple" [0,1,2,3,4]
-- *Main> pluralize "apple" [0,1,2,3,4]
-- ["no apples","one apple","2 apples","3 apples","4 apples"]

