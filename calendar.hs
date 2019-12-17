data Day = Sunday
         | Monday
         | Tuesday
         | Wednesday
         | Thursday
         | Friday
         | Saturday
         deriving(Show, Eq, Ord)

data Month = January
           | February
           | March
           | April
           | May
           | June
           | July
           | August
           | September
           | October
           | November
           | December
           deriving(Show, Eq, Ord)

data Holiday = Thanksgiving
             | MothersDay
             | FathersDay
             deriving(Show, Eq, Ord)


firstDay :: Int -> (Maybe Day)
firstDay x | x >= 1 = Just (getDay (calculateFirst x))
firstDay x | x < 1  = Nothing

anyDate :: Int -> Int -> Int -> (Maybe Day)
anyDate m d y | y >= 1 = Just (getDay (calculateDate m d y))
              | y < 1 = Nothing
              | (not (mod y 4 == 0)) = Nothing

getDay :: Int -> Day
getDay x | x == 0 = Sunday
getDay x | x == 1 = Monday
getDay x | x == 2 = Tuesday
getDay x | x == 3 = Wednesday
getDay x | x == 4 = Thursday
getDay x | x == 5 = Friday
getDay x | x == 6 = Saturday

getMonth :: Month -> Int
getMonth x | x == January = 11
getMonth x | x == February = 12
getMonth x | x == March = 1
getMonth x | x == April = 2
getMonth x | x == May = 3
getMonth x | x == June = 4
getMonth x | x == July = 5
getMonth x | x == August = 6
getMonth x | x == September = 7
getMonth x | x == October = 8
getMonth x | x == November = 9
getMonth x | x == December = 10


calculateFirst :: Int -> Int
calculateFirst year = mod (1 + 5*(mod (year-1) 4) + 4*(mod (year-1) 100) + 6*(mod (year-1) 400)) 7


calculateDate :: Int -> Int -> Int -> Int
calculateDate m d y | (m >= 3)  = (mod(6 + d + ceiling(2.6*fromIntegral(m-2)) + 5*(mod y 4) + 4*(mod y 100) + 6*(mod y 400)) 7)
calculateDate m d y | (m == 2) =  (mod(6 + d + ceiling(2.6*fromIntegral(12)) + 5*(mod (y-1) 4) + 4*(mod (y-1) 100) + 6*(mod (y-1) 400)) 7)
calculateDate m d y | (m == 1) = (mod(6 + d + ceiling(2.6*fromIntegral(11)) + 5*(mod (y-1) 4) + 4*(mod (y-1) 100) + 6*(mod (y-1) 400)) 7)

whenIs :: Holiday -> Int -> Int
whenIs holiday year | holiday == Thanksgiving = 29 - ((calculateDate 09 01 year) + 1)
whenIs holiday year | holiday == MothersDay = 15 - ((calculateDate 10 01 year) + 1)
whenIs holiday year | holiday == FathersDay = 22 - ((calculateDate 11 01 year) + 1)

printMonth :: Month -> Int -> IO ()
