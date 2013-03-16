calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w,h) <- xs]
	where bmi weight height = weight / height ^ 2

-- here, "bmi" is a function
-- *Main> calcBmis [(85,1.9), (90,1.5)]
-- [23.545706371191137,40.0]
