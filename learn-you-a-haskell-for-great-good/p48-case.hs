-- head' :: [a] -> a
-- head' [] = error "No head for empty list"
-- head' (x:_) = x

-- ... same code by case

head' :: [a] -> a
head' xs = case xs of []    -> error "No head for empty list"
                      (x:_) -> x

-- 引数パターンマッチは関数定義の中だけしか使えないがcaseはどこでも。

describeList :: [a] -> String
describeList ls = "The list is "
                  ++ case ls of [] = "empty"
                               [x] = "a singleton list."
                                xs = "a longer list"

-- or, 

describeList :: [a] -> String
describeList ls = "The list is " ++ what ls
    where what [] = "empty"
          what [x] = "a singleton list."
          what xs = "a longer list"

