getElTengah :: [Int] -> Int
{- getElTengah l mengembalikan elemen l yang berada di tengah-tengah.
 Apabila elemen l berjumlah genap (misalnya n), maka yang dikembalikan adalah elemen pada
 posisi (n div 2).
 Prekondisi: list tidak kosong
 Contoh: getElTengah [3,2,6,5,8] = 6; getElTengah [1,2,3,4] = 2 -}

isEmpty :: [Int] -> Bool
isOneElmt :: [Int] -> Bool
konso :: Int -> [Int] -> [Int]


--Realisasi
isEmpty l = null l
isOneElmt l = (length l) == 1
konso e li = [e] ++ li

getElTengah li = if isEmpty li then 0
                else if isOneElmt li then (head li)
                else
                    if isOneElmt (tail li) then (head li)
                    else getElTengah (tail (init li))