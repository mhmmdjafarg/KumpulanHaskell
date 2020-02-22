isEmpty :: [Int] -> Bool
-- isEmpty l  true jika list of integer l kosong
-- REALISASI
isEmpty l = null l

isOneElmt :: [Int] -> Bool
-- isOneElmt l true jika list of integer l hanya mempunyai satu elemen
-- REALISASI
isOneElmt l = (length l) == 1 

-- DEFINISI DAN SPESIFIKASI KONSTRUKTOR
konso :: Int -> [Int] -> [Int]
{- konso e li menghasilkan sebuah list of integer dari e (sebuah integer) dan li 
   (list of integer), dengan e sebagai elemen pertama: e o li -> li' -}
-- REALISASI
konso e li = [e] ++ li


elPosGanjil :: [Int] -> [Int]
{- elPosGanjil l mengembalikan sebuah list yang berisi semua elemen l pada posisi ganjil.
 Contoh: elPosGanjil [2,4,6,8,1,3,5,7,9] = [2,6,1,5,9] -}
 
elPosGanjil li = if (isEmpty li) then []
				     else konso (head li) ( elPosGanjil (tail(tail li)))
