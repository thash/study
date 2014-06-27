palindrome :: [a] -> [a]
palindrome lst = iter lst []
  where
    iter [] xs = xs
    iter (x:xs) stack = x:iter xs (x:stack)
