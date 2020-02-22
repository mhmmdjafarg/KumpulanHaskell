{- Membuat prosedur filterlist yang menerima input list kemudian memisahkan sesuai parameter yang dipilih-}

filterlist :: [Int] -> (Int -> Bool) -> [Int]

isEmpty :: [Int] -> Bool
isEmpty l = null l

konso :: Int -> [Int] -> [Int]
konso e l = [e] ++ l


-- Fungsi filter
isPos :: Int -> Bool
isNeg :: Int -> Bool
isKabisat :: Int -> Bool





-- Fungsi isPos
isPos x = if x > 0 then True
		  else False
		  
-- Fungsi isNeg
isNeg x = if x < 0 then True
		  else False
		  
-- Fungsi isKabisat
isKabisat x = if ((((mod x 4 == 0) && (mod x 100 /= 0))) || ((mod x 100 == 0) && (mod x 400) == 0) ) then True
			  else False
		
filterlist l fungsi = if isEmpty l then []
					 else 
						if fungsi (head l) then konso (head l) ( filterlist (tail l) fungsi)
						else (filterlist (tail l) fungsi)
