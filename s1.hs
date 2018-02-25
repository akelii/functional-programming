{-
	Elif AK
	150130009
	26.02.2018
	
	Functional Programming Exercise #1 in Haskell
-}

{-
	Part #1
	```````
	~> 'dayOfWeek' implementation uses Zeller's congruence which takes 
	year, month and day respectively. Note that:
		(0= Saturday, 1= Sunday, 2= Monday, ... , 6= Friday) and
		(3= March, 4= April, 5= May, ... , 14= February)
	so that if month is January or February then it is summed with 12,
	and year is substracted by 1. (Zeller's implementation) 
	The python implementation and javascript implementation on
	Zeller's congruence do not include the 'year substraction'. I add the
	substraction according to Zeller's formula.
	If it is not added, the result will not correct for January and February.
-}
dayOfWeek :: Integer -> Integer -> Integer -> Integer
dayOfWeek y m d = z `mod` 7
	where
		y'= takeNewY m y		
		k = y' `mod` 100
		j = y' `div` 100
		m'= takeNewM m
		t1 = floor (fromIntegral (13 * (m' + 1)) / 5.0)
		t2 = floor (fromIntegral k / 4.0)
		t3 = floor (fromIntegral j / 4.0)
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
	Part #2
	```````
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
	Part #3
	```````
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
	Part #4
	```````
	~> "sundays2" calculates number of sundays without Zeller's congruence
	math formula. It uses "leap" and  "daysInMonth" functions.
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
	
sundays2 :: Integer -> Integer -> Integer
sundays2 start end = sundays' start 1 2
	where
		sundays' :: Integer -> Integer -> Integer -> Integer
		sundays' y m ds --year, month, days(weekday)
			| y > end 	= 0
			| otherwise	= if days `mod` 7 == 0 then rest + 1 else rest
			where
				nextY 	= if m == 12 then y + 1 else y
				nextM 	= if m == 12 then 1 else m + 1
				days 	= ds + (daysInMonth m y) `mod` 7
				rest	= sundays' nextY nextM days
				
{-
	Part #5
	```````
	~> If the non centrury year is divisible by 4, then it is a leap year.
	For example, in 100 years, there should be 25 leap years like 4, 8, 12, ... , 100.
	However for the centrury year, if it is divisible by 400, it is a leap year;
	otherwise not. So 100 is not divisible by 400, it is not leap year.
	
	In 100 years, there are 25-1=24 leap years. And in 400 year, 4*24 = 96;
	and +1 year for 400th year (it is divisible by 400). Totally, 97 leap years occurs.
	
	Every 400 years, there are (400*365 = 146,000) + 97 days -> 146097 days and
	there are 20871 weeks (146097 is divisible by 7)
	
	As a result, each days of week repeat 20871 times (total num of weeks).
	In other words, sunday occurs 20871 times in 400 year period.
	And same way, other days like monday, tuesday ... etc occurs 20871 times in 400 years.
	
	For any day of month, the probobilty, which certain day of month is a 
	sunday or any day, will be 1/7. Bacause as I said, number of 'days of week'
	are equal: 20871 in 146097.
	
	But another view is that, all 'day of month' are not equal. For example,
	29th February exists 97 times in 400 year; however normal days like 23th April
	exists 400 times in 400 years. So it breaks the possibility of 1/7. 
	Even if we have equal number of 'day of week', we have not equal 'day of month'.
	In this point, I thought that there is a  inconsistency in the question.
	
	As I said, normal days like 23th April exists 400 times in 400 years and
	400 is not divisible by 7. So same 'day of month' (lets say 23th April) 
	could not appear every 'day of week' in same probability, even if total
	number of days (146097) is divisible by 7. 
-}
