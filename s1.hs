{-
	Question 1
	``````````
	~> 'dayOfWeek' implementation uses Zeller's congruence which takes 
	year, month and day respectively. Note that:
		(0= Saturday, 1= Sunday, 2= Monday, ... , 6= Friday) and
		(3= March, 4= April, 5= May, ... , 14= February)
	so that if month is January or February then it is summed with 12,
	and year is substracted by 1. (Zeller's implementation) 
-}
dayOfWeek :: Integer -> Integer -> Integer -> Integer
dayOfWeek y m d = z `mod` 7
	where
		y'= takeNewY m y		
		k = y' `mod` 100
		j = y' `div` 100
		m'= takeNewM m
		t1 = floor (fromIntegral (13 * (m' + 1)) / 5.0)
		t2 = floor (fromIntegral (k) / 4.0)
		t3 = floor (fromIntegral (j) / 4.0)
		z  = d + t1 + k + t2 + t3 + 5 * j
		takeNewM :: Integer -> Integer
		takeNewM m
			| m <= 2 	= m + 12
			| otherwise	= m	
		takeNewY :: Integer -> Integer -> Integer
		takeNewY m y
			| m <= 2 	= y - 1
			| otherwise	= y			






