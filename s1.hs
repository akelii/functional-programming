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

{-
	Question 2
	``````````
	~> What does the helper function (sundays') calculate?
		:: (sundays') calculates number of sundays in certain month and 
		certain year. But it is recursive function, so it repeats this 
		process until reach base case which is upper bound of the year (end). 
		As a result, (sundays1) return all sundays count in given year interval. 
		
	~> What if you don't define a "rest" and use its expression where it's needed?
		:: Doesn' matter. We can use as its expression instead of "rest" without 
		thinking parenthesis (operation priority). It works as same way.
-}
sundays1 :: Integer -> Integer -> Integer
sundays1 start end = sundays' start 1
	where
		sundays' :: Integer -> Integer -> Integer
		sundays' y m
			| y > end 	= 0
			| otherwise	= if dayOfWeek y m 1 == 1 then rest + 1 else rest
			where
				nextY 	= if m == 12 then y + 1 else y
				nextM 	= if m == 12 then 1 else m + 1
				rest	= sundays' nextY nextM
				
{-
	Question 3
	``````````
	~> tail recursive function of 'sundays1' is implemented below.
	Accumulator carries the last result at the end of the recursion
-}
tailSundays1 :: Integer -> Integer -> Integer
tailSundays1 start end = sundays' 0 start 1
	where
		sundays' :: Integer -> Integer -> Integer -> Integer
		sundays' acc y m
			| y > end 	= acc
			| otherwise	= if dayOfWeek y m 1 == 1 then sundays' (acc + 1) nextY nextM else sundays' acc nextY nextM 
			where
				nextY 	= if m == 12 then y + 1 else y
				nextM 	= if m == 12 then 1 else m + 1

{-
	Question 4
	``````````
	
-}
leap :: Integer -> Bool
leap y
	| y `mod` 400 	== 0 = True
	| y `mod` 100 	== 0 = False
	| y `mod` 4		== 0 = True
	| otherwise			 = False

daysInMonth :: Integer -> Integer -> Integer
daysInMonth m y = case m of
	4  -> 30
	6  -> 30
	9  -> 30
	11 -> 30
	2  -> if leap y == True then 29 else 28
	_  -> 31


