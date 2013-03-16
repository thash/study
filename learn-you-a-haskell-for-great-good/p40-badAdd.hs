badAdd :: (Num a) => [a] -> a
badAdd (x:y:z:[]) = x + y + z
